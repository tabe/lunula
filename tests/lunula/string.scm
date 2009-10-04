#!/usr/bin/env ypsilon
#!r6rs

(import (lunula string) (xunit))

(assert-string=? "call_with_current_continuation" (string-underscore "call-with-current-continuation"))

(assert-string=? "call-with-current-continuation" (string-hyphen "call_with_current_continuation"))

(assert-boolean=? #t (blank? #f))
(assert-boolean=? #t (blank? ""))
(assert-boolean=? #f (blank? 0))
(assert-boolean=? #f (blank? "0"))

(assert-string=? "abc" (string-truncate "abc" 4))
(assert-string=? "abc" (string-truncate "abc" 3))
(assert-string=? "ab..." (string-truncate "abc" 2))
(assert-string=? "a..." (string-truncate "abc" 1))

(report)
