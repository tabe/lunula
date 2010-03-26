#!r6rs

(import (rnrs)
        (ypsilon socket)
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

(let* ((port "3024")
       (server (make-server-socket port))
       (client (make-client-socket "localhost" port)))
  (default-handler client '())
  (socket-close client)
  (let* ((sock (socket-accept server))
         (data (socket-recv sock 1024 0)))
    (assert-bytevector=? #vu8(83 116 97 116 117 115 10 52 48 52 32 78 111 116 32 70 111 117 110 100 10 67 111 110 116 101 110 116 45 84 121 112 101 10 116 101 120 116 47 104 116 109 108 59 32 99 104 97 114 115 101 116 61 85 84 70 45 56 10 67 111 110 116 101 110 116 45 76 101 110 103 116 104 10 48 10 101 110 100 10)
                         data)
    (socket-close sock)
    (socket-close server)))

(report)
