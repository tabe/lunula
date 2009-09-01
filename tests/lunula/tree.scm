#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula tree)
        (xunit))

(assert-string=? "abcdefg123" (tree->string '("" () "a" b ((#\c "def") "g" 123) )))
(assert-string=? "abc100%" (tree->string `(abc ,(lambda (a b) (+ a b)) "%") 20 80))
(assert-string=? "vector" (tree->string '#(v e c t o r)))

(report)
