#!r6rs

(import (rnrs (6))
        (only (core parameters) parameterize)
        (lunula path)
        (xunit))

(define *dummy-validator* (lambda (ht) (lambda args #t)))

(api-set! 'test-api *dummy-validator* 'test values)

(assert-boolean=? #f (api-path? #f))
(assert-boolean=? #f (api-path? ""))
(assert-boolean=? #f (api-path? "/"))
(assert-boolean=? #f (api-path? "//"))
(assert-equal? '("a" "b" "c") (cdr (api-path? "/test-api/a/b/c.html")))

(scenario-set! 'test-scenario values)

(assert-boolean=? #f (entry-path? ""))
(assert-boolean=? #f (entry-path? "/"))
(assert-boolean=? #f (entry-path? "//"))
(assert-boolean=? #f (entry-path? "/test-scenario"))
(assert-procedure? (entry-path? "/test-scenario.html"))

(assert-string=? "/test-api.html" (build-api-path 'test-api #f))
(assert-string=? "/test-api.html?abc" (build-api-path 'test-api "abc"))
(assert-string=? "/test-api/a/b/c.html" (build-api-path 'test-api #f "a" "b" "c"))
(assert-string=? "/test-api/a/b/c.html?abc" (build-api-path 'test-api "abc" "a" "b" "c"))

(assert-string=? "/test-scenario.html" (build-entry-path 'test-scenario))
(assert-string=? "/test-scenario.html" (build-entry-path 'test-scenario #f))
(assert-string=? "/test-scenario.html?xyz" (build-entry-path 'test-scenario "xyz"))

(let ((path (generate-temporary-path)))
  (assert-string? path)
  (assert-boolean=? #f (consume-temporary-path! path))
  (provide-temporary-path! path)
  (assert-boolean=? #t (consume-temporary-path! path))
  (assert-boolean=? #f (consume-temporary-path! path)))

(scenario-clear!)

(parameterize ((path-extension ".xhtml")
               (path-transformer (lambda (x) (if (null? x) '() (cdr x))))
               (path-builder (lambda (x) (string-append "/prefixed" x))))

  (api-set! 'test-api *dummy-validator* 'test values)

  (assert-boolean=? #f (api-path? #f))
  (assert-boolean=? #f (api-path? ""))
  (assert-boolean=? #f (api-path? "/"))
  (assert-boolean=? #f (api-path? "//"))
  (assert-boolean=? #f (api-path? "/prefixed/"))
  (assert-equal? '("a" "b" "c") (cdr (api-path? "/prefixed/test-api/a/b/c.xhtml")))

  (scenario-set! 'test-scenario values)

  (assert-boolean=? #f (entry-path? ""))
  (assert-boolean=? #f (entry-path? "/"))
  (assert-boolean=? #f (entry-path? "//"))
  (assert-boolean=? #f (entry-path? "/test-scenario"))
  (assert-procedure? (entry-path? "/prefixed/test-scenario.xhtml"))

  (assert-string=? "/prefixed/test-api.xhtml" (build-api-path 'test-api #f))
  (assert-string=? "/prefixed/test-api.xhtml?abc" (build-api-path 'test-api "abc"))
  (assert-string=? "/prefixed/test-api/a/b/c.xhtml" (build-api-path 'test-api #f "a" "b" "c"))
  (assert-string=? "/prefixed/test-api/a/b/c.xhtml?abc" (build-api-path 'test-api "abc" "a" "b" "c"))

  (assert-string=? "/prefixed/test-scenario.xhtml" (build-entry-path 'test-scenario))
  (assert-string=? "/prefixed/test-scenario.xhtml" (build-entry-path 'test-scenario #f))
  (assert-string=? "/prefixed/test-scenario.xhtml?xyz" (build-entry-path 'test-scenario "xyz")))

(report)
