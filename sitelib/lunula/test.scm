(library (lunula test)
  (export assert-=
          assert-string=?
          report)
  (import (core)
          (rnrs))

  (define result (make-parameter '()))

  (define-syntax assert-equivalence
    (syntax-rules ()
      ((_ equiv expected exp)
       (let ((actual exp))
         (guard (con
                 ((assertion-violation? con)
                  (result
                   (cons (format "~s expected, but ~s => ~s~%"
                                 expected
                                 'exp
                                 actual)
                         (result)))))
           (assert (equiv expected actual)))))))

  (define-syntax assert-=
    (syntax-rules ()
      ((_ expected exp)
       (assert-equivalence = expected exp))))

  (define-syntax assert-string=?
    (syntax-rules ()
      ((_ expected exp)
       (assert-equivalence string=? expected exp))))

  (define (report)
    (cond ((null? (result))
           (display "passed.\n")
           (exit))
          (else
           (for-each
            (lambda (e)
              (display e (current-error-port)))
            (result))
           (display "failed.\n")
           (exit 1))))

)
