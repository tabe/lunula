#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula mysql)
        (only (lunula persistent-record) define-persistent-record-type persistent-protocol id-of)
        (prefix (only (lunula log) info) log:)
        (xunit))

(define-persistent-record-type account
  (fields name password)
  (protocol
   (persistent-protocol
    (lambda (p)
      (lambda (x y)
        (p x y))))))

(define a (make-account "lol" "________"))

(connect "localhost" "root" "yoursql" "errata")

(let ((x (save a)))
  (assert-boolean=? #t x))
(let ((x (lookup account (id-of a))))
  (assert (account? x))
  (assert-= (id-of a) (id-of x))
  (assert-string=? "lol" (account-name x))
  (assert-string=? "________" (account-password x)))
(let ((x (destroy a)))
  (assert-= 1 x))
(let ((x (destroy a)))
  (assert-= 0 x))

(log:info "~s" (lookup-all account '()))

(close)

(report)
