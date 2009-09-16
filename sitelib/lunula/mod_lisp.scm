(library (lunula mod_lisp)
  (export page
          form
          mail
          redirect
          start
          define-scenario
          do-login
          do-logout
          logged-in?
          path-extension
          add-input-fields
          templates
          template-environment
          build-entry-path
          content->alist
          entry-paths)
  (import (core)
          (concurrent)
          (match)
          (rnrs)
          (only (srfi :1) unfold take drop)
          (srfi :8)
          (only (srfi :13) string-prefix-ci? string-suffix? string-tokenize)
          (only (srfi :14) char-set char-set-complement)
          (srfi :19)
          (srfi :48)
          (ypsilon socket)
          (prefix (uri) uri:)
          (lunula concurrent)
          (only (lunula gettext) ___)
          (prefix (lunula html) html:)
          (prefix (lunula log) log:)
          (lunula sendmail)
          (lunula session)
          (lunula tree)
          (lunula uri))

  (define *timeout* (* 5 60 1000))

  (define *scenario* (make-hashtable string-hash string=?))

  (define (entry-paths) (hashtable-keys *scenario*))

  (define *response* (make-messenger-bag 10))

  (define *request* (make-messenger-bag 10))

  (define path-extension (make-parameter ".html"))

  (define *temporary-path* (make-messenger-bag 10))

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

  (define templates (make-parameter (lookup-process-environment "LUNULA_TEMPLATES")))

  (define *template-environment*
    (make-parameter
     '((except (rnrs) div)
       (lunula html))))

  (define-syntax template-environment
    (syntax-rules ()
      ((_ e0 e1 ...)
       (*template-environment* '(e0 e1 ...)))))

  (define-syntax eval-template
    (syntax-rules ()
      ((_ template)
       (let ((forest (call-with-input-file (format "~a/~a.scm" (templates) template)
                       (lambda (port)
                         (unfold eof-object?
                                 values
                                 (lambda _ (read port))
                                 (read port))))))
         (eval (cons 'list forest) (apply environment (*template-environment*)))))))

  (define-syntax make-html
    (syntax-rules ()
      ((_ template body ...)
       (begin
         (log:info "lunula> template: ~a" template)
         (log:info "lunula> args: ~s" (list body ...))
         (tree->string (eval-template template) body ...)))))

  (define (input-title rtd name)
    (___ (string->symbol (format "~a-~a" (record-type-name rtd) name))))

  (define (input-field type name v)
    (cond ((list? type)
           (match type
             (('radio . ls)
              (map
               (lambda (x)
                 (let ((id (make-uuid)))
                   (append (html:input ((type 'radio) (name name) (value (car x)) (id id) (checked (caddr x))))
                           (html:label ((for id)) (cadr x)))))
               ls))
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
       (let* ((path (string-append "/" (make-uuid) (path-extension)))
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

  (define (entry-path? path)
    (hashtable-ref *scenario* path #f))

  (define (send-header&content client header . content)
    (let* ((h (fold-left (lambda (x y) (format "~a~a~%~a~%" x (car y) (cdr y))) "" header)))
      (socket-send client (string->utf8 (string-append h "end\n")) 0)
      (unless (null? content)
        (socket-send client (car content) 0))))

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

  (define-condition-type &malformed-key-value &condition
    make-malformed-key-value malformed-key-value?
    (kv malformed-key-value-kv-of))

  (define (content->alist content)
    (map
     (lambda (kv)
       (match (string-tokenize kv (char-set-complement (char-set #\=)))
         ((k v)
          (cons (string->symbol k) (uri:decode-string v 'application/x-www-form-urlencoded)))
         ((k)
          (cons (string->symbol k) ""))
         (_
          (raise (make-malformed-key-value kv)))))
     (string-tokenize content (char-set-complement (char-set #\&)))))

  (define-syntax form
    (syntax-rules ()
      ((_ (io sess) (record-name record) template message ...)
       (receive (rtd path body)
           (make-form (record-name record) message ...)
         (let ((f (future
                   (messenger-bag-put! *temporary-path* path #t)
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
                    (messenger-bag-get-gracefully! *temporary-path* path 100)
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
       (let* ((path (string-append "/" (make-uuid) (path-extension)))
              (f (future
                  (cond ((zero? (call-with-values (lambda () (composer path)) sendmail))
                         (messenger-bag-put! *temporary-path* path #t)
                         (match (messenger-bag-get! *request* path (* 2 *timeout*))
                           ((header content) #t)
                           (_ #f)))
                        (else #f)))))
         (let ((uuid (and (session? sess) (session-uuid sess))))
           (messenger-bag-put! *response* (recv io) `(200 template ,uuid ,message)))
         (send io path)
         (let ((result (f *timeout*)))
           (cond ((timeout-object? result)
                  (messenger-bag-get-gracefully! *temporary-path* path 100)
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

  (define-condition-type &premature-end-of-header &condition
    make-premature-end-of-header premature-end-of-header?)

  (define (binary-get-line port)
    (let lp ((x '()))
      (let ((b (get-u8 port)))
        (cond ((eof-object? b)
               (raise (make-premature-end-of-header)))
              ((= 10 b)
               (utf8->string (u8-list->bytevector (reverse x))))
              (else
               (lp (cons b x)))))))

  (define (binary-port->header port)
    (assert (binary-port? port))
    (unfold null?
            (lambda (x) (take x 2))
            (lambda (x) (drop x 2))
            (unfold (lambda (x) (cond ((eof-object? x)
                                       (close-port port)
                                       (raise (make-premature-end-of-header)))
                                      (else
                                       (string=? "end" x))))
                    (lambda (x) x)
                    (lambda (x) (binary-get-line port))
                    (binary-get-line port))))

  (define-condition-type &too-big-header &condition
    make-too-big-header too-big-header?
    (data too-big-header-data-of))

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
                        (raise (make-too-big-header data))))))
          (call-with-port (open-bytevector-input-port data)
            (lambda (port)
              (let ((header (binary-port->header port)))
                (values header (get-bytevector-all port)))))))))

  (define-condition-type &missing-method &condition
    make-missing-method missing-method?
    (header missing-method-header-of))

  (define (method-of header)
    (let ((x (assoc "method" header)))
      (if x
          (cadr x)
          (raise (make-missing-method header)))))

  (define-condition-type &missing-content-length &condition
    make-missing-content-length missing-content-length?
    (header missing-content-length-header-of))

  (define (content-length-of header)
    (let ((x (or (assoc "content-length" header)
                 (assoc "Content-Length" header))))
      (if x
          (string->number (cadr x))
          (raise (make-missing-content-length header)))))

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

  (define (path-filter path)
    (and (string? path)
         (if (string=? "/" path) "/index.html" path)))

  (define (start port)
    (collect-notify #t)
    (spawn-heap-limit (* 6 1024 1024))
    (let ((socket (make-server-socket port)))
      (let lp ((client (socket-accept socket)))
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
            (let ((path (path-filter (path-of header))))
              (cond ((entry-path? path)
                     =>
                     (lambda (proc)
                       (log:info "lunula> entry: ~a" path)
                       (spawn* (lambda ()
                                 (log:info "lunula> here")
                                 (guard (con
                                         ((message-condition? con)
                                          (log:info "lunula> ~a" (condition-message con))
                                          con)
                                         ((timeout-object? con)
                                          con))
                                   (proc header content)))
                               (lambda (x)
                                 (log:info "lunula> there: ~a" x)))
                       (let ((response (messenger-bag-get! *response* path)))
                         (log:info "lunula> response: ~s" response)
                         (send-response client response))))
                    ((messenger-bag-get-gracefully! *temporary-path* path 100 #f)
                     (log:info "lunula> temp: ~a" path)
                     (messenger-bag-put! *request* path (list header content))
                     (let ((response (messenger-bag-get! *response* path)))
                       (log:info "lunula> response: ~s" response)
                       (send-response client response)))
                    (else
                     (log:info "lunula> default: ~a" path)
                     (default-handler header client))))
            (socket-close client)
            (lp (socket-accept socket)))))))

  (define (build-entry-path name . query)
    (let ((path (string-append "/" (symbol->string name) (path-extension))))
      (cond ((null? query)
             path)
            ((not (car query))
             path)
            (else
             (apply string-append path "?" query)))))

  (define-syntax define-scenario
    (syntax-rules ()
      ((_ (name io request data) e0 e1 ...)
       (define name
         (let ((path (build-entry-path 'name)))
           (define (proc header content)
             (let ((io (make-mailbox))
                   (request header)
                   (data content))
               (dynamic-wind
                   (lambda () (send io path))
                   (lambda () e0 e1 ...)
                   (lambda () (shutdown-mailbox io)))))
           (hashtable-set! *scenario* path proc)
           proc)))
      ((_ (name io request) e0 e1 ...)
       (define name
         (let ((path (build-entry-path 'name)))
           (define (proc header content)
             (let ((io (make-mailbox))
                   (request header))
               (dynamic-wind
                   (lambda () (send io path))
                   (lambda () e0 e1 ...)
                   (lambda () (shutdown-mailbox io)))))
           (hashtable-set! *scenario* path proc)
           proc)))))

)
