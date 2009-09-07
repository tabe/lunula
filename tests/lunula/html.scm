#!/usr/bin/env ypsilon
#!r6rs

(import (lunula html) (lunula tree) (xunit))

(assert-string=? "<br />" (tree->string (br)))
(assert-string=? "<hr />" (tree->string (hr)))
(assert-string=? "<form method='POST'></form\n>"
                 (tree->string (form)))
(assert-string=? "<textarea rows='5' cols='50'></textarea\n>"
                 (tree->string (textarea)))
(assert-string=? "<span>abc</span\n>"
                 (tree->string (span "abc")))
(assert-string=? "<script type='text/javascript'>alert('hello world')</script\n>"
                 (tree->string (script ((type "text/javascript")) "alert" #\( "'hello world'" #\))))
(assert-string=? "<input type='radio' name='foo' value='bar' checked />"
                 (tree->string (input ((type "radio") (name "foo") (value "bar") (checked #t)))))
(assert-string=? "<input type='radio' name='foo' value='bar' />"
                 (tree->string (input ((type "radio") (name "foo") (value "bar") (checked #f)))))

(assert-char=? #\a (escape-char #\a))
(assert-string=? "&amp;" (escape-char #\&))
(assert-string=? "&quot;" (escape-char #\"))
(assert-string=? "&#039;" (escape-char #\'))
(assert-string=? "&lt;" (escape-char #\<))
(assert-string=? "&gt;" (escape-char #\>))
(assert-string=? "&lt;script type=&quot;text/javascript&quot;&gt;" (escape-string "<script type=\"text/javascript\">"))

(report)
