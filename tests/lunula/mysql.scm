#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula mysql)
        (only (lunula configuration) define-configuration)
        (only (lunula persistent-record) define-persistent-record-type persistent-protocol id-of)
        (prefix (only (lunula log) info) log:)
        (xunit))

(define-persistent-record-type foobar
  (fields (mutable name) (mutable memo))
  (protocol
   (persistent-protocol
    (lambda (p)
      (lambda (x y)
        (p x y))))))

(define-syntax foobar-is
  (syntax-rules ()
    ((_ id name memo)
     (let ((x (lookup foobar id)))
       (assert-boolean=? #t (foobar? x))
       (assert-= id (id-of a))
       (assert-string=? name (foobar-name x))
       (assert-string=? memo (foobar-memo x))))))

(define a (make-foobar "lol" "________"))

(define-configuration mysql-user)
(define-configuration mysql-password)
(define-configuration mysql-database)

(connect "localhost" mysql-user mysql-password mysql-database)

(assert-boolean=? #t (save a))
(foobar-is (id-of a) "lol" "________")
(foobar-name-set! a #f)
(foobar-memo-set! a "let me see ...")
(assert-= 1 (save a))
(foobar-is (id-of a) "lol" "let me see ...")
(let ((x (destroy a)))
  (assert-= 1 x))
(let ((x (destroy a)))
  (assert-= 0 x))

(assert-= 0 (execute "BEGIN"))
(assert-= 0 (execute "INSERT INTO foobar (name, memo) VALUES ('temporary', 'temporary')"))
(let ((len (length (lookup-all foobar '()))))
  (assert-= 1 len))
(assert-= 0 (execute "DELETE FROM foobar"))
(let ((len (length (lookup-all foobar '()))))
  (assert-= 0 len))
(assert-= 0 (execute "COMMIT"))

(close)

(report)
