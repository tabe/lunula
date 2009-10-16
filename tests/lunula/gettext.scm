#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula gettext)
        (xunit))

(gettext
 (abc (en "abc")
      (ja "ａｂｃ"))
 (hello-world (en "Hello, world.")
              (ja "こんにちは、世界。"))
 )

(assert-eq? 'en (locale))
(assert-string=? "abc" (__ abc))
(assert-string=? "abc" (___ 'abc))
(assert-string=? "Hello, world." (__ hello-world))
(assert-string=? "Hello, world." (___ 'hello-world))
(assert-string=? "foo" (__ foo))
(assert-string=? "foo" (___ 'foo))

(locale ja)
(assert-eq? 'ja (locale))

(assert-string=? "ａｂｃ" (__ abc))
(assert-string=? "ａｂｃ" (___ 'abc))
(assert-string=? "こんにちは、世界。" (__ hello-world))
(assert-string=? "こんにちは、世界。" (___ 'hello-world))
(assert-string=? "foo" (__ foo))
(assert-string=? "foo" (___ 'foo))

(report)
