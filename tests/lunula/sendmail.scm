#!/usr/bin/env ypsilon
#!r6rs

(import (lunula configuration)
        (lunula sendmail)
        (xunit))

(define-configuration test-mail-address)

(skip-unless test-mail-address
  (assert-zero? (sendmail test-mail-address test-mail-address "test for (lunula sendmail)" "あいうえお")))

(report)
