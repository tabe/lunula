#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula mysql)
        (xunit))

(define-record-type account
  (fields id name password)
  (protocol
   (lambda (p)
     (lambda (x y z)
       (p (if (string? x) (string->number x) x) y z)))))

(define a (make-account 101 "lol" "________"))

(connect "localhost" "root" "yoursql" "errata")

(let ((x (save a)))
  (assert-boolean=? #t x))
(let ((x (lookup account 101)))
  (assert (account? x))
  (assert-= 101 (account-id x))
  (assert-string=? "lol" (account-name x))
  (assert-string=? "________" (account-password x)))
(let ((x (destroy a)))
  (assert-= 1 x))
(let ((x (destroy a)))
  (assert-= 0 x))

(write (lookup-all account '()))
(newline)

(close)

(report)
