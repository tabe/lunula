#!r6rs

(import (rnrs)
        (lunula controller)
        (only (lunula path) api-path? entry-path?)
        (xunit))

(define-api (test-api x y z)
  (lambda (ht) (lambda args #t))
  test-api
  #t)

(define-api (unavailable-api x y z)
  (lambda (ht) (lambda args (hashtable-set! ht 'unavailable "unavailable")))
  unavailable-api
  #t)

(define-scenario (entry-0 io request)
  0)

(define-scenario (entry-1 io request data)
  1)

(let ((x (api-path? "/test-api.html")))
  (assert-list? x)
  (assert-= 1 (length x))
  (assert-equal? '() (cdr x)))
(let ((x (api-path? "/test-api/a/b.html")))
  (assert-list? x)
  (assert-= 3 (length x))
  (assert-equal? '("a" "b") (cdr x)))
(let ((x (api-path? "/test-api/a/b/c.html")))
  (assert-list? x)
  (assert-= 4 (length x))
  (assert-equal? '("a" "b" "c") (cdr x)))
(let ((x (api-path? "/test-api/a/b/c/d.html")))
  (assert-list? x)
  (assert-= 5 (length x))
  (assert-equal? '("a" "b" "c" "d") (cdr x)))
(assert-boolean=? #f (api-path? "/unavailable-api/a/b/c.html"))

(let ((x (entry-path? "/entry-0.html")))
  (assert-procedure? x)
  (assert-= 0 (x #f '() #f)))

(let ((x (entry-path? "/entry-1.html")))
  (assert-procedure? x)
  (assert-= 1 (x #f '() #f)))

(report)
