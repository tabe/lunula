#!/usr/bin/env ypsilon
#!r6rs

(import (lunula sendmail) (xunit))

(assert-= 0 (sendmail "tabe" "tabe" "test for (lunula sendmail)" "あいうえお"))

(report)
