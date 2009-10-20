(library (lunula controller)
  (export page
          form
          mail
          redirect
          start
          define-api
          define-scenario
          do-login
          do-logout
          logged-in?
          add-input-fields)
  (import (only (core) collect-notify display-thread-status format make-parameter usleep)
          (only (concurrent) future make-mailbox make-messenger-bag make-uuid messenger-bag-get! messenger-bag-put! recv send shutdown-mailbox spawn* spawn-heap-limit timeout-object?)
          (match)
          (rnrs)
          (only (srfi :1) iota)
          (srfi :8)
          (srfi :19)
          (srfi :48)
          (only (ypsilon socket) make-server-socket socket-accept socket-close socket-recv socket-send)
          (lunula concurrent)
          (only (lunula gettext) ___)
          (prefix (lunula html) html:)
          (prefix (lunula log) log:)
          (only (lunula mod_lisp) get-header premature-end-of-header? put-header)
          (only (lunula path) api-component-procedure api-component-template api-path? api-set! build-entry-path consume-temporary-path! entry-path? generate-temporary-path provide-temporary-path! scenario-set!)
          (only (lunula request) content->alist content-length-of method-of parameter-of path-of)
          (lunula sendmail)
          (only (lunula session) do-login do-logout logged-in? session-uuid session?)
          (only (lunula template) load-templates template->tree)
          (lunula tree))

  (define *timeout* (* 5 60 1000))

  (define *response* (make-messenger-bag 10))

  (define *request* (make-messenger-bag 10))

  (define *input-types* (make-eq-hashtable 10))

  (define *input-descriptions* (make-eq-hashtable 10))

  (define-syntax split-input-specification
    (syntax-rules ()
      ((_ (t d) () proc)
       (proc (reverse t) (reverse d)))
      ((_ (t d) (#f e ...) proc)
       (split-input-specification ((cons #f t) (cons #f d)) (e ...) proc))
      ((_ (t d) ((type) e ...) proc)
       (split-input-specification ((cons 'type t) (cons #f d)) (e ...) proc))
      ((_ (t d) ((type desc) e ...) proc)
       (split-input-specification ((cons 'type t) (cons desc d)) (e ...) proc))))

  (define-syntax add-input-fields
    (syntax-rules ()
      ((_ record-name (e ...))
       (split-input-specification
        ('() '())
        (e ...)
        (lambda (t d)
          (let ((rtd (record-type-descriptor record-name)))
            (hashtable-set! *input-types* rtd t)
            (hashtable-set! *input-descriptions* rtd d)))))))

  (define (input-types rtd)
    (hashtable-ref *input-types* rtd '()))

  (define (input-descriptions rtd)
    (hashtable-ref *input-descriptions* rtd '()))

  (define-syntax make-html
    (syntax-rules ()
      ((_ template body ...)
       (tree->string (template->tree template) body ...))))

  (define (input-title rtd name)
    (___ (string->symbol (format "~a-~a" (record-type-name rtd) name))))

  (define (radio-buttons name v ls)

    (define (radio-button x y)
      (let ((id (make-uuid)))
        (list (html:input ((type 'radio) (name name) (value (car x)) (id id) (checked y)))
              (html:label ((for id)) (cadr x))
              "&nbsp;")))

    (let ((checked (map (lambda (x) (string=? (format "~a" v) (format "~a" (car x)))) ls)))
      (if (exists values checked)
          (map radio-button ls checked)
          (map (lambda (x) (radio-button x (caddr x))) ls))))

  (define (input-field type name v)
    (cond ((list? type)
           (match type
             (('radio . ls)
              (radio-buttons name v ls))
             (('select . ls)
              (html:select
               ((name name))
               (map
                (lambda (x)
                  (let ((selected (string=? (format "~a" v) (format "~a" (car x)))))
                    (html:option ((value (car x)) (selected selected)) (cadr x))))
                ls)))))
          ((eq? 'textarea type)
           (html:textarea ((name name)) v))
          ((symbol? type)
           (case type
             ((password) (html:input ((type type) (name name))))
             (else (html:input ((type type) (name name) (value v))))))
          (else (error 'input-field "invalid type" (list name type v)))))

  (define-syntax make-form
    (syntax-rules ()
      ((_ (record-name record) message ...)
       (let* ((path (generate-temporary-path))
              (rtd (if (record? record) (record-rtd record) (record-type-descriptor record-name)))
              (names (record-type-field-names rtd)))
         (values
          rtd
          path
          (tree->string
           (list
            message ...
            (html:form
             ((action path))
             (html:table
              ()
              (fold-left (lambda (s name type desc i)
                           (define (row type v)
                             (html:tr
                              (html:th (input-title rtd name))
                              (html:td (input-field type name v)
                                       (if desc
                                           (append (html:br) (html:span ((class "description")) desc))
                                           '()))))
                           (if type
                               (cond ((record? record)
                                      (let ((v ((record-accessor rtd i) record)))
                                        (if (eq? 'id name)
                                            (if (integer? v)
                                                (cons
                                                 s
                                                 (row 'hidden v))
                                                s)
                                            (cons
                                             s
                                             (row type v)))))
                                     ((eq? 'id name)
                                      s)
                                     (else
                                      (cons
                                       s
                                       (row type ""))))
                               s))
                         '()
                         (vector->list names)
                         (input-types rtd)
                         (input-descriptions rtd)
                         (iota (vector-length names))))
             (html:input ((type "submit") (name "submit") (value (___ 'submit))))
             "&nbsp;"
             (html:input ((type "submit") (name "cancel") (value (___ 'cancel))))))
           path))))))

  (define (send-header&content client header . content)
    (let ((h (call-with-string-output-port (lambda (port) (put-header port header)))))
      (socket-send client (string->utf8 h) 0))
    (unless (null? content)
      (socket-send client (car content) 0)))

  (define (send-html client template uuid body)
    (let* ((html (make-html template uuid body))
           (content (string->utf8 html)))
      (send-header&content client
                           `(("Status" . "200 OK")
                             ("Content-Type" . "text/html; charset=UTF-8")
                             ("Content-Length" . ,(bytevector-length content)))
                           content)))

  (define-syntax page
    (syntax-rules ()
      ((_ (io sess) template message)
       (let ((uuid (and (session? sess) (session-uuid sess))))
         (messenger-bag-put! *response* (recv io) `(200 template ,uuid ,message))))
      ((_ (io) template message)
       (page (io #f) template message))
      ((_ param template)
       (page param template '()))))

  (define-syntax form
    (syntax-rules ()
      ((_ (io sess) (record-name record) template message ...)
       (receive (rtd path body)
           (make-form (record-name record) message ...)
         (let ((f (future
                   (provide-temporary-path! path)
                   (match (messenger-bag-get! *request* path (* 2 *timeout*))
                     ((header content)
                      (let ((alist (content->alist content)))
                        (cond ((assoc 'submit alist)
                               (vector-map
                                (lambda (name)
                                  (cond ((assoc name alist) => cdr)
                                        (else #f)))
                                (record-type-field-names rtd)))
                              (else #f))))
                     (_ #f)))))
           (let ((uuid (and (session? sess) (session-uuid sess))))
             (messenger-bag-put! *response* (recv io) `(200 template ,uuid ,body)))
           (send io path)
           (let ((fields (f *timeout*)))
             (cond ((timeout-object? fields)
                    (consume-temporary-path! path)
                    (messenger-bag-get-gracefully! *request* path 100)
                    (raise fields))
                   ((vector? fields)
                    (log:info "lunula> fields: ~s" fields)
                    (apply (record-constructor (record-constructor-descriptor record-name))
                           (vector->list fields)))
                   (else #f))))))
      ((_ (io sess) (record-name) template messenge ...)
       (form (io sess) (record-name #f) template messenge ...))
      ((_ (io) (record-name record) template messenge ...)
       (form (io #f) (record-name record) template messenge ...))
      ((_ (io) (record-name) template messenge ...)
       (form (io #f) (record-name #f) template messenge ...))))

  (define-syntax mail
    (syntax-rules ()
      ((_ (io sess) template message composer)
       (let* ((path (generate-temporary-path))
              (f (future
                  (cond ((zero? (call-with-values (lambda () (composer path)) sendmail))
                         (provide-temporary-path! path)
                         (match (messenger-bag-get! *request* path (* 2 *timeout*))
                           ((header content) #t)
                           (_ #f)))
                        (else #f)))))
         (let ((uuid (and (session? sess) (session-uuid sess))))
           (messenger-bag-put! *response* (recv io) `(200 template ,uuid ,message)))
         (send io path)
         (let ((result (f *timeout*)))
           (cond ((timeout-object? result)
                  (consume-temporary-path! path)
                  (messenger-bag-get-gracefully! *request* path 100)
                  #f)
                 (else result)))))
      ((_ (io) template message composer)
       (mail (io #f) template message composer))))

  (define-syntax redirect
    (syntax-rules ()
      ((_ (io sess) path)
       (let* ((p path)
              (x (cond ((session? sess)
                        (if (symbol? p)
                            (build-entry-path p (session-uuid sess))
                            (string-append path "?" (session-uuid sess))))
                       ((symbol? p)
                        (build-entry-path p))
                       (else p))))
         (messenger-bag-put! *response* (recv io) `(302 ,x))))
      ((_ (io) path)
       (redirect (io #f) path))))

  (define (send-response client response)
    (match response
      ((200 template uuid body)
       (send-html client template uuid body))
      ((302 url)
       (send-header&content client
                            `(("Status" . "302 Found")
                              ("Location" . ,url))))))

  (define (default-handler header client)
    (let ((content (string->utf8 (make-html 404))))
      (send-header&content client
                           `(("Status" . "404 Not Found")
                             ("Content-Type" . "text/html; charset=UTF-8")
                             ("Content-Length" . ,(bytevector-length content)))
                           content)))

  (define *header-buffer-size* (* 32 1024))

  (define-condition-type &too-big-header &condition
    make-too-big-header too-big-header?)

  (define (receive-header client)
    (let ((data (socket-recv client *header-buffer-size* 0)))
      (let lp ((data data)
               (dlen (bytevector-length data)))
        (guard (con
                ((premature-end-of-header? con)
                 (cond ((< dlen *header-buffer-size*)
                        (let ((next (socket-recv client (- *header-buffer-size* dlen) 0)))
                          (cond ((eof-object? next)
                                 (usleep 500000)
                                 (lp data dlen))
                                (else
                                 (let* ((nlen (bytevector-length next))
                                        (dlen+nlen (+ dlen nlen))
                                        (bv (make-bytevector dlen+nlen)))
                                   (bytevector-copy! data 0 bv 0 dlen)
                                   (bytevector-copy! next 0 bv dlen nlen)
                                   (lp bv dlen+nlen))))))
                       (else
                        (raise (condition
                                (make-too-big-header)
                                (make-irritants-condition data)))))))
          (call-with-port (open-bytevector-input-port data)
            (lambda (port)
              (let ((header (get-header port)))
                (values header (get-bytevector-all port)))))))))

  (define (utf8-list->string utf8-list length)
    (let* ((bv (make-bytevector length))
           (len (fold-left (lambda (n x)
                             (let ((len (bytevector-length x)))
                               (bytevector-copy! x 0 bv n len)
                               (+ n len)))
                           0
                           utf8-list)))
      (assert (= len length))
      (utf8->string bv)))

  (define (receive-content client prefix content-length)
    (define (lp leadings rest-length)
      (let* ((rest (socket-recv client rest-length 0))
             (len (bytevector-length rest)))
        (cond ((= len rest-length)
               (utf8-list->string (reverse (cons rest leadings)) content-length))
              (else
               (assert (< len rest-length))
               (lp (cons rest leadings) (- rest-length len))))))
    (cond ((< 0 content-length)
           (cond ((eof-object? prefix)
                  (lp '() content-length))
                 (else
                  (let ((len (bytevector-length prefix)))
                    (cond ((= content-length len)
                           (utf8->string prefix))
                          (else
                           (assert (< len content-length))
                           (lp (list prefix) (- content-length len))))))))
          (else "")))

  (define (start port)
    (collect-notify #t)
    (spawn-heap-limit (* 6 1024 1024))
    (load-templates #t)
    (let ((socket (make-server-socket port)))
      (let lp ((client (socket-accept socket)))
        (define (clean-up x)
          (log:info "lunula> ~a" x)
          (socket-close client))
        (load-templates #f)
        (receive (header rest)
            (receive-header client)
          (log:info "lunula> header: ~s" header)
          (let* ((method (method-of header))
                 (content (cond ((string-ci=? "POST" method)
                                 (let* ((content-length (content-length-of header))
                                        (content (receive-content client rest content-length)))
                                   (log:info "lunula> content: ~s" content)
                                   content))
                                (else #f))))
            ;;
            (display-thread-status)
            ;;
            (let ((path (path-of header)))
              (cond ((entry-path? path)
                     => (lambda (proc)
                          (let ((uuid (make-uuid)))
                            (spawn* (lambda ()
                                      (guard (con
                                              ((message-condition? con)
                                               (log:info "lunula> ~a" (condition-message con))
                                               con)
                                              ((timeout-object? con)
                                               con))
                                        (proc uuid header content)))
                                    (lambda (x) (log:info "lunula> ~a" x)))
                            (spawn* (lambda ()
                                      (let ((response (messenger-bag-get! *response* uuid)))
                                        (log:info "lunula> response: ~s" response)
                                        (send-response client response)))
                                    clean-up))))
                    ((api-path? path)
                     => (lambda (pair)
                          (spawn* (lambda ()
                                    (let ((component (car pair)))
                                      (cond ((apply (api-component-procedure component) (cdr pair))
                                             => (lambda (body)
                                                  (send-html client
                                                             (api-component-template component)
                                                             (cond ((logged-in? (parameter-of header)) => session-uuid)
                                                                   (else #f))
                                                             body)))
                                            (else (default-handler header client)))))
                                  clean-up)))
                    ((consume-temporary-path! path)
                     (spawn* (lambda ()
                               (messenger-bag-put! *request* path (list header content))
                               (let ((response (messenger-bag-get! *response* path)))
                                 (log:info "lunula> response: ~s" response)
                                 (send-response client response)))
                             clean-up))
                    (else
                     (spawn* (lambda () (default-handler header client))
                             clean-up))))))
        (lp (socket-accept socket)))))

  (define-syntax define-scenario
    (syntax-rules ()
      ((_ (name io request data) e0 e1 ...)
       (define name
         (letrec ((proc
                   (lambda (uuid header content)
                     (let ((io (make-mailbox))
                           (request header)
                           (data content))
                       (dynamic-wind
                           (lambda () (send io uuid))
                           (lambda () e0 e1 ...)
                           (lambda () (shutdown-mailbox io)))))))
           (scenario-set! 'name proc)
           proc)))
      ((_ (name io request) e0 e1 ...)
       (define name
         (letrec ((proc (lambda (uuid header content)
                          (let ((io (make-mailbox))
                                (request header))
                            (dynamic-wind
                                (lambda () (send io uuid))
                                (lambda () e0 e1 ...)
                                (lambda () (shutdown-mailbox io)))))))
           (scenario-set! 'name proc)
           proc)))))

  (define-syntax define-api
    (syntax-rules ()
      ((_ (name arg ...) validator template e0 e1 ...)
       (let ((proc (lambda (arg ...) e0 e1 ...)))
         (api-set! 'name validator 'template proc)))))

)
