#!/usr/bin/env ypsilon
#!r6rs

(import (lunula tree)
        (lunula xml)
        (xunit))

(define-element foo)
(define-element/ fooo)

(assert-string=? "<?xml version='1.0' encoding='UTF-8'?>\n" (tree->string (declaration 1.0 "UTF-8")))

(assert-string=? "<foo baz='' />" (tree->string (foo ((bar #f) (baz "")))))
(assert-string=? "<foo bar='1' baz>quux</foo\n>" (tree->string (foo ((bar "1") (baz #t)) "quux")))
(assert-string=? "<fooo></fooo\n>" (tree->string (fooo)))

(assert-char=? #\a (escape-char #\a))
(assert-char=? #\あ (escape-char #\あ))
(assert-string=? "&amp;" (escape-char #\&))
(assert-string=? "&quot;" (escape-char #\"))
(assert-string=? "&#039;" (escape-char #\'))
(assert-string=? "&lt;" (escape-char #\<))
(assert-string=? "&gt;" (escape-char #\>))

(assert-string=? "" (escape-string ""))
(assert-string=? "\r\n" (escape-string "\r\n"))
(assert-string=? "&lt;script&gt;alert(&#039;&quot;&amp;&quot;&#039;);&lt;/script&gt;"
                 (escape-string "<script>alert('\"&\"');</script>"))

(report)
