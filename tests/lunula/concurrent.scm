#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs) (concurrent) (lunula concurrent))

(define bag (make-messenger-bag 10))

(assert (condition? (messenger-bag-get-gracefully! bag "x" 100)))
(assert (not (messenger-bag-get-gracefully! bag "x" 100 #f)))

(messenger-bag-put! bag "x" 'foo 100)
(assert (eq? 'foo (messenger-bag-get-gracefully! bag "x" 100)))
