(library (lunula validation)
  (export define-validator
          define-string-length-validator
          define-composite-validator
          guide
          hashtable->messages)
  (import (rnrs)
          (only (lunula gettext) ___)
          (prefix (lunula html) html:))

  (define-syntax define-validator
    (syntax-rules ()
      ((_ (name arg ...) (message ...) body ...)
       (define (name ht)
         (lambda (arg ...)
           (define message
             (lambda params
               (hashtable-set! ht 'message params)))
           ...
           body ...)))))

  (define-syntax define-string-length-validator
    (syntax-rules ()
      ((_ name (is-blank too-short too-long) (min max))
       (define-validator (name str)
         (is-blank too-short too-long)
         (let ((len (string-length str)))
           (cond ((zero? len) (is-blank))
                 ((< len min) (too-short))
                 ((< max len) (too-long))))))
      ((_ name (is-blank too-long) (max))
       (define-validator (name str)
         (is-blank too-long)
         (let ((len (string-length str)))
           (cond ((zero? len) (is-blank))
                 ((< max len) (too-long))))))
      ((_ name (too-long) (max))
       (define-validator (name str)
         (too-long)
         (when (< max (string-length str)) (too-long))))))

  (define-syntax define-composite-validator
    (syntax-rules ()
      ((_ name (extractor validator ...) ...)
       (define (name ht)
         (lambda args
           (call-with-values
               (lambda () (apply extractor args))
             (lambda params
               (apply (validator ht) params)
               ...))
           ...)))))

  (define-syntax guide
    (syntax-rules ()
      ((_ (validator x ...) guidance follow-up)
       (let ((ht (make-eq-hashtable)))
         (call-with-values
             (lambda () ((validator ht) x ...))
           (lambda r
             (cond ((zero? (hashtable-size ht))
                    (apply follow-up r))
                   (else
                    (guidance ht)))))))))

  (define (hashtable->messages ht)
    (html:ul (vector-map (lambda (k) (html:li (___ k))) (hashtable-keys ht))))

)
