#!/usr/bin/env ypsilon
#!r6rs

(import (lunula configuration)
        (xunit))

(define-configuration foo)
(define-configuration bar "fallback")

(assert-boolean=? #f foo)
(assert-string=? "fallback" bar)

(report)
