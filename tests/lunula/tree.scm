#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula tree)
        (lunula test))

(assert-string=? "abcdefg123" (tree->string '("" () "a" b ((#\c "def") "g" 123) )))

(report)
