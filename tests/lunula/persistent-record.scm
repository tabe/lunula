#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula persistent-record)
        (xunit))

(define-assert-predicate persistent-record?)

(define-persistent-record-type foo
  (fields x y z)
  (protocol
   (persistent-protocol
    (lambda (p)
      (lambda (a b c)
        (p a b c))))))

(let ((p (make-persistent-record 7 #f #f)))
  (assert-persistent-record? p)
  (assert-= 7 (id-of p))
  (assert-boolean=? #f (created-at-of p))
  (assert-boolean=? #f (updated-at-of p)))

(let ((q (make-persistent-record "100" "2009-10-16 19:20:00" "2009-10-16 19:30:00")))
  (assert-persistent-record? q)
  (assert-= 100 (id-of q))
  (assert-string=? "2009-10-16 19:20:00" (created-at-of q))
  (assert-string=? "2009-10-16 19:30:00" (updated-at-of q)))

(let ((f (make-foo 1 2 3)))
  (assert-boolean=? #f (id-of f))
  (assert-boolean=? #f (created-at-of f))
  (assert-boolean=? #f (updated-at-of f))
  (assert-= 1 (foo-x f))
  (assert-= 2 (foo-y f))
  (assert-= 3 (foo-z f))
  (let ((id 12))
    (id-set! f id)
    (assert-= id (id-of f)))
  (let ((t "2009-10-16 00:00:00"))
    (created-at-set! f t)
    (assert-string=? t (created-at-of f)))
  (let ((t "2009-10-16 12:00:00"))
    (updated-at-set! f t)
    (assert-string=? t (updated-at-of f))))

(report)
