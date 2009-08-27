#!/usr/bin/env ypsilon
#!r6rs

(import (lunula html) (xunit))

(assert-char=? #\a (escape-char #\a))
(assert-string=? "&amp;" (escape-char #\&))
(assert-string=? "&quot;" (escape-char #\"))
(assert-string=? "&#039;" (escape-char #\'))
(assert-string=? "&lt;" (escape-char #\<))
(assert-string=? "&gt;" (escape-char #\>))
(assert-string=? "&lt;script type=&quot;text/javascript&quot;&gt;" (escape-string "<script type=\"text/javascript\">"))

(report)
