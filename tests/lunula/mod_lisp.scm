#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula mod_lisp)
        (xunit))

(define *dummy-validator* (lambda (ht) (lambda args #t)))

(define-api (test x y z)
  *dummy-validator*
  test
  #t)

(assert-equal? '("a" "b" "c") (cdr (api-path? "/test/a/b/c.html")))
  
(report)
