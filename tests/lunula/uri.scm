#!/usr/bin/env ypsilon
#!r6rs

(import (lunula uri) (xunit))

(assert-string=? "/path/example" (url->path "/path/example#abc"))
(assert-string=? "abc" (url->fragment "/path/example#abc"))

(report)
