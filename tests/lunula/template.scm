#!r6rs

(import (rnrs)
        (only (lunula gettext) gettext)
        (lunula template)
        (xunit))

(gettext
 (hello-world (en "Hello, world.")
              (ja "こんにちは、世界。"))
)

(templates "templates")
(load-templates #t)
(assert-equal? '("404 Not Found") (template->tree 404 'en))
(assert-equal? '() (template->tree 'empty 'en))
(assert-equal? '(()) (template->tree 'null 'en))
(assert-equal? '("こんにちは、世界。") (template->tree 'hello-world 'ja))
(assert-equal? '("Hello, world.") (template->tree 'hello-world 'en))
(delete-file "templates/00TOUCHED")

(report)
