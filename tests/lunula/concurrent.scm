#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (only (concurrent) make-messenger-bag messenger-bag-put!)
        (lunula concurrent)
        (xunit))

(define bag (make-messenger-bag 10))

(assert-condition? (messenger-bag-get-gracefully! bag "x" 100))
(assert-boolean=? #f (messenger-bag-get-gracefully! bag "x" 100 #f))

(messenger-bag-put! bag "x" 'foo 100)
(assert-eq? 'foo (messenger-bag-get-gracefully! bag "x" 100))

(report)
