(library (lunula request)
  (export content-length-of
          method-of
          invalid-content-length?
          missing-content-length?
          missing-method?)
  (import (rnrs))

  (define-condition-type &missing-method &condition
    make-missing-method missing-method?)

  (define (method-of header)
    (cond ((assoc "method" header) => cadr)
          (else (raise
                 (condition
                  (make-missing-method)
                  (make-irritants-condition header))))))

  (define-condition-type &invalid-content-length &condition
    make-invalid-content-length invalid-content-length?)

  (define-condition-type &missing-content-length &condition
    make-missing-content-length missing-content-length?)

  (define (raise-invalid-content-length irritants)
    (raise
     (condition
      (make-invalid-content-length)
      (make-irritants-condition irritants))))

  (define (content-length-of header)
    (cond ((or (assoc "content-length" header)
               (assoc "Content-Length" header))
           => (lambda (x)
                (cond ((string->number (cadr x))
                       => (lambda (n)
                            (cond ((negative? n)
                                   (raise-invalid-content-length n))
                                  ((fixnum? n)
                                   n)
                                  (else
                                   (raise-invalid-content-length n)))))
                      (else (raise-invalid-content-length (cadr x))))))
          (else (raise
                 (condition
                  (make-missing-content-length)
                  (make-irritants-condition header))))))

)
