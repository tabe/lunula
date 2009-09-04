(library (lunula validation)
  (export define-validator
          define-composite-validator
          guide)
  (import (rnrs))

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

  (define-syntax define-composite-validator
    (syntax-rules ()
      ((_ name (extractor sub ...) ...)
       (define (name ht)
         (lambda args
           (call-with-values
               (lambda () (apply extractor args))
             (lambda params
               (apply (sub ht) params)
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

)