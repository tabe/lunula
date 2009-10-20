(library (lunula controller)
  (export default-handler
          page
          form
          mail
          redirect
          define-api
          define-scenario
          add-input-fields
          send-html
          send-response)
  (import (rnrs)
          (only (core) make-parameter usleep)
          (only (ypsilon concurrent) future make-mailbox make-uuid messenger-bag-get! messenger-bag-put! recv send shutdown-mailbox timeout-object?)
          (match)
          (only (srfi :1) iota)
          (only (srfi :8) receive)
          (srfi :19)
          (only (srfi :28) format)
          (srfi :48)
          (only (ypsilon socket) socket-send)
          (only (lunula concurrent) messenger-bag-get-gracefully!)
          (only (lunula gettext) ___)
          (prefix (lunula html) html:)
          (prefix (lunula log) log:)
          (only (lunula mod_lisp) put-header)
          (only (lunula path) api-set! build-entry-path consume-temporary-path! generate-temporary-path provide-temporary-path! scenario-set!)
          (only (lunula pipeline) request-drop! request-get! response-put!)
          (only (lunula request) content->alist)
          (only (lunula sendmail) sendmail)
          (only (lunula session) session-uuid session?)
          (only (lunula template) template->tree)
          (only (lunula tree) tree->string))

  (define *timeout* (* 5 60 1000))

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
         (response-put! (recv io) `(200 template ,uuid ,message))))
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
                   (match (request-get! path (* 2 *timeout*))
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
             (response-put! (recv io) `(200 template ,uuid ,body)))
           (send io path)
           (let ((fields (f *timeout*)))
             (cond ((timeout-object? fields)
                    (consume-temporary-path! path)
                    (request-drop! path)
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
                         (match (request-get! path (* 2 *timeout*))
                           ((header content) #t)
                           (_ #f)))
                        (else #f)))))
         (let ((uuid (and (session? sess) (session-uuid sess))))
           (response-put! (recv io) `(200 template ,uuid ,message)))
         (send io path)
         (let ((result (f *timeout*)))
           (cond ((timeout-object? result)
                  (consume-temporary-path! path)
                  (request-drop! path)
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
         (response-put! (recv io) `(302 ,x))))
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
