#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula template)
        (xunit))

(templates "templates")
(load-templates #t)
(assert-equal? '("404 Not Found") (template->tree 404))
(assert-equal? '() (template->tree 'empty))
(assert-equal? '(()) (template->tree 'null))

(report)
