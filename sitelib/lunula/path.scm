(library (lunula path)
  (export api-component-procedure
          api-component-template
          api-component-validator
          api-path?
          api-set!
          build-api-path
          build-entry-path
          consume-temporary-path!
          entry-path?
          entry-paths
          generate-temporary-path
          make-api-component
          path-extension
          provide-temporary-path!
          scenario-set!)
  (import (rnrs)
          (only (core) make-parameter)
          (only (srfi :1) append-map)
          (only (srfi :13) string-suffix? string-tokenize)
          (only (srfi :14) char-set char-set-complement)
          (only (ypsilon concurrent) make-messenger-bag messenger-bag-put! make-uuid)
          (only (uri) decode-string)
          (only (lunula concurrent) messenger-bag-get-gracefully!)
          (only (lunula validation) guide))

  (define path-extension (make-parameter ".html"))

  (define *scenario* (make-hashtable string-hash string=?))

  (define (entry-paths) (hashtable-keys *scenario*))

  (define *api* (make-eq-hashtable 10))

  (define-record-type api-component
    (fields validator template procedure))

  (define *temporary-path* (make-messenger-bag 10))

  (define (entry-path? path)
    (hashtable-ref *scenario* path #f))

  (define (api-path? path)
    (and (string? path)
         (let ((ext (path-extension)))
           (and (string-suffix? ext path)
                (let ((ls (string-tokenize (substring path 0 (- (string-length path) (string-length ext)))
                                           (char-set-complement (char-set #\/)))))
                  (if (null? ls)
                      #f
                      (cond ((hashtable-ref *api* (string->symbol (car ls)) #f)
                             => (lambda (component)
                                  (let ((args (map decode-string (cdr ls))))
                                    (guide ((api-component-validator component) args)
                                      (lambda _ #f)
                                      (lambda _ (cons component args))))))
                            (else #f))))))))

  (define (build-api-path name query . args)
    (let ((last (cons
                 (path-extension)
                 (if (string? query)
                     (list "?" query)
                     '()))))
      (cond ((for-all string? args)
             (apply string-append
                    (cons*
                     "/"
                     (symbol->string name)
                     (append
                      (append-map (lambda (arg) (list "/" arg)) args)
                      last))))
            (else (apply string-append "/" last)))))

  (define (build-entry-path name . query)
    (let ((path (string-append "/" (symbol->string name) (path-extension))))
      (cond ((null? query) path)
            ((for-all string? query) (apply string-append path "?" query))
            (else path))))

  (define (api-set! name validator template proc)
    (hashtable-set! *api* name (make-api-component validator template proc)))

  (define (scenario-set! name proc)
    (hashtable-set! *scenario* (build-entry-path name) proc))

  (define (generate-temporary-path)
    (string-append "/" (make-uuid) (path-extension)))

  (define (provide-temporary-path! path)
    (messenger-bag-put! *temporary-path* path #t))

  (define (consume-temporary-path! path)
    (messenger-bag-get-gracefully! *temporary-path* path 100 #f))

)
