#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula uri)
        (lunula test))

(assert-string=? "/path/example" (url->path "/path/example#abc"))
(assert-string=? "abc" (url->fragment "/path/example#abc"))

(assert-string=? " " (decode-string "%20"))
(assert-string=? "/path/example" (decode-string "/path/example"))
(assert-string=? "省メモリプログラミング" (decode-string "%e7%9c%81%e3%83%a1%e3%83%a2%e3%83%aa%e3%83%97%e3%83%ad%e3%82%b0%e3%83%a9%e3%83%9f%e3%83%b3%e3%82%b0"))
(assert-string=? "%20" (encode-string " "))
(assert-string=? "%e3%81%82%e3%81%84%e3%81%86%e3%81%88%e3%81%8a" (encode-string "あいうえお"))

(report)
