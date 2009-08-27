#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula uri)
        (lunula test))

(assert-string=? "/path/example" (url->path "/path/example#abc"))
(assert-string=? "abc" (url->fragment "/path/example#abc"))

(report)
