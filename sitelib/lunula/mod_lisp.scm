(library (lunula mod_lisp)
  (export page
          form
          redirect
          start
          define-scenario
          do-login
          do-logout
          logged-in?)
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
          (lunula concurrent)
          (prefix (lunula html) html:)
          (lunula session)
          (lunula tree)
          (prefix (lunula uri) uri:))

  (define *timeout* (* 30 1000))

  (define *scenario* '())

  (define *response* (make-messenger-bag 10))

  (define *request* (make-messenger-bag 10))

  (define *temporary-path* (make-messenger-bag 10))

  (define-syntax make-html
    (syntax-rules ()
      ((_ title body ...)
       (tree->string
        (list
         (html:doctype 'transitional)
         (html:html
          (html:head
           (html:title title))
          (html:body body ...)))))
      ((_ body)
       (tree->string
        (list
         (html:doctype 'transitional)
         (html:html
          (html:head)
          (html:body body)))))))

  (define-syntax make-form
    (syntax-rules ()
      ((_ (record-name record) message ...)
       (let* ((path (string-append "/" (make-uuid)))
              (rtd (if (record? record) (record-rtd record) (record-type-descriptor record-name)))
              (names (record-type-field-names rtd)))
         (values
          rtd
          path
          (string-append
           message ...
           (format "<form action='~a' method='POST'>~%" path) ;; FIXME
           (fold-left (lambda (s name i)
                        (cond ((record? record)
                               (let ((v ((record-accessor rtd i) record)))
                                 (if (eq? 'id name)
                                     (if (integer? v)
                                         (format "~a<p>~a: <input type='hidden' name='~a' value='~a' /></p>~%"
                                                 s name name v)
                                         s)
                                     (format "~a<p>~a: <input type='text' name='~a' value='~a' /></p>~%"
                                             s name name v))))
                              ((eq? 'id name)
                               s)
                              (else
                               (format "~a<p>~a: <input type='text' name='~a' /></p>~%" s name name))))
                      ""
                      (vector->list names)
                      (iota (vector-length names)))
           "<input type='submit' value='submit' />\n</form>"))))))

  (define *static-path*
    '("/" "/favicon.ico"))

  (define (static-path? path)
    (member path *static-path*))

  (define (entry-path? path)
    (let ((pair (assoc path *scenario*)))
      (and pair (cdr pair))))

  (define (send-header&content client header . content)
    (let* ((h (fold-left (lambda (x y) (format "~a~a~%~a~%" x (car y) (cdr y))) "" header)))
      (socket-send client (string->utf8 (string-append h "end\n")) 0)
      (unless (null? content)
        (socket-send client (car content) 0))))

  (define (links uuid)
    (let ((x (fold-left (lambda (x pair)
                          (let ((path (car pair)))
                            (if (string? uuid)
                                (format "~a<li><a href='~a?~a'>~a</a></li>~%" x path uuid path)
                                (format "~a<li><a href='~a'>~a</a></li>~%" x path path))))
                        ""
                        *scenario*)))
      (format "<ul>~%~a</ul>~%" x)))

  (define (send-html client body uuid)
    (let* ((html (make-html "typo" body (links uuid)))
           (content (string->utf8 html)))
      (send-header&content client
                           `(("Status" . "200 OK")
                             ("Content-Type" . "text/html; charset=UTF-8")
                             ("Content-Length" . ,(bytevector-length content)))
                           content)))

  (define-syntax page
    (syntax-rules ()
      ((_ (io sess) message ...)
       (let ((uuid (and (session? sess) (session-uuid sess))))
         (messenger-bag-put! *response* (recv io) `(200 ,(format "<p>~a</p>" (format message ...)) ,uuid))))
      ((_ (io) message ...)
       (page (io #f) message ...))))

  (define-condition-type &malformed-key-value &condition
    make-malformed-key-value malformed-key-value?
    (kv malformed-key-value-kv-of))

  (define (content->alist content)
    (map
     (lambda (kv)
       (match (string-tokenize kv (char-set-complement (char-set #\=)))
         ((k v)
          (cons (string->symbol k) (uri:decode-string v)))
         ((k)
          (cons (string->symbol k) ""))
         (else
          (raise (make-malformed-key-value kv)))))
     (string-tokenize content (char-set-complement (char-set #\&)))))

  (define-syntax form
    (syntax-rules ()
      ((_ (io sess) (record-name record) message ...)
       (receive (rtd path body)
           (make-form (record-name record) message ...)
         (let ((f (future
                   (messenger-bag-put! *temporary-path* path #t)
                   (match (messenger-bag-get! *request* path (* 2 *timeout*))
                     ((header content)
                      (vector-map
                       (lambda (name)
                         (let ((x (assoc name (content->alist content))))
                           (and x (cdr x))))
                       (record-type-field-names rtd)))))))
           (let ((uuid (and (session? sess) (session-uuid sess))))
             (messenger-bag-put! *response* (recv io) `(200 ,body ,uuid)))
           (send io path)
           (let ((fields (f *timeout*)))
             (cond ((timeout-object? fields)
                    (messenger-bag-get-gracefully! *temporary-path* path 100)
                    (messenger-bag-get-gracefully! *request* path 100)
                    (raise fields))
                   (else
                    (format #t "fields: ~s~%" fields)
                    (apply (record-constructor (record-constructor-descriptor record-name))
                           (vector->list fields))))))))
      ((_ (io sess) (record-name) messenge ...)
       (form (io sess) (record-name #f) messenge ...))
      ((_ (io) (record-name record) messenge ...)
       (form (io #f) (record-name record) messenge ...))
      ((_ (io) (record-name) messenge ...)
       (form (io #f) (record-name #f) messenge ...))))

  (define-syntax redirect
    (syntax-rules ()
      ((_ (io) path)
       (messenger-bag-put! *response* (recv io) `(302 ,path)))))

  (define (send-response client response)
    (match response
      ((200 body uuid)
       (send-html client body uuid))
      ((302 url)
       (send-header&content client
                            `(("Status" . "302 Found")
                              ("Location" . ,url))))))

  (define (static-handler header client)
    (let ((x (uri:parameter-of header)))
      (send-html client "<p>asdf,fdsa<p>" x)))

  (define (default-handler header client)
    (let ((content (string->utf8 (make-html "404 Not Found"))))
      (send-header&content client
                           `(("Status" . "404 Not Found")
                             ("Content-Type" . "text/html; charset=UTF-8")
                             ("Content-Length" . ,(bytevector-length content)))
                           content)))

  (define *header-buffer-size* 1024)

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

  (define (receive-header client)
    (let ((data (socket-recv client *header-buffer-size* 0)))
      (let lp ((data data)
               (dlen (bytevector-length data)))
        (guard (con
                ((premature-end-of-header? con)
                 ;; (format (current-error-port) "premature-end-of-header: ~a~&" (utf8->string data))
                 (let* ((next (socket-recv client (- *header-buffer-size* dlen) 0))
                        (nlen (bytevector-length next))
                        (dlen+nlen (+ dlen nlen))
                        (bv (make-bytevector dlen+nlen)))
                   (bytevector-copy! data 0 bv 0 dlen)
                   (bytevector-copy! next 0 bv dlen nlen)
                   (lp bv dlen+nlen))))
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
    (assert (< 0 content-length))
    (cond ((eof-object? prefix)
           (lp '() content-length))
          (else
           (let ((len (bytevector-length prefix)))
             (cond ((= content-length len)
                    (utf8->string prefix))
                   (else
                    (assert (< len content-length))
                    (lp (list prefix) (- content-length len))))))))

  (define (start port)
    (collect-notify #t)
    (spawn-heap-limit (* 6 1024 1024))
    (let ((socket (make-server-socket port)))
      (let lp ((client (socket-accept socket)))
        (receive (header rest)
            (receive-header client)
          (format #t "header: ~s~%" header)
          (let* ((method (method-of header))
                 (content (cond ((string-ci=? "POST" method)
                                 (let* ((content-length (content-length-of header))
                                        (content (receive-content client rest content-length)))
                                   (format #t "content: ~s~%" content)
                                   content))
                                (else #f))))
            (flush-output-port (current-output-port))
            ;;
            (display-thread-status)
            ;;
            (let ((path (uri:path-of header)))
              (cond ((static-path? path)
                     (format (current-error-port) "static: ~a~%" path)
                     (static-handler header client))
                    ((entry-path? path)
                     =>
                     (lambda (proc)
                       (format (current-error-port) "entry: ~a~%" path)
                       (spawn* (lambda ()
                                 (display "here\n" (current-error-port))
                                 (guard (con
                                         ((timeout-object? con)
                                          con))
                                   (proc header)))
                               (lambda (x)
                                 (format (current-error-port) "there: ~a~&" x)))
                       (let ((response (messenger-bag-get! *response* path)))
                         (format (current-error-port) "response: ~s~&" response)
                         (send-response client response))))
                    ((messenger-bag-get-gracefully! *temporary-path* path 100 #f)
                     (format (current-error-port) "temp: ~a~%" path)
                     (messenger-bag-put! *request* path (list header content))
                     (let ((response (messenger-bag-get! *response* path)))
                       (format (current-error-port) "response: ~s~&" response)
                       (send-response client response)))
                    (else
                     (format (current-error-port) "default: ~a~%" path)
                     (default-handler header client))))
            (socket-close client)
            (lp (socket-accept socket)))))))

  (define-syntax define-scenario
    (syntax-rules ()
      ((_ (name io request) e0 e1 ...)
       (define name
         (let ((path (string-append "/" (symbol->string 'name))))
           (define (proc header)
             (let ((io (make-mailbox))
                   (request header))
               (dynamic-wind
                   (lambda () (send io path))
                   (lambda () e0 e1 ...)
                   (lambda () (shutdown-mailbox io)))))
           (set! *scenario* (cons (cons path proc) *scenario*))
           proc)))))

)
