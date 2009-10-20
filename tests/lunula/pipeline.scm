#!r6rs

(import (rnrs)
        (lunula pipeline)
        (xunit))

(request-put! "/some-path.html" '(() #f))
(assert-equal? '(() #f)
               (request-get! "/some-path.html" 1000))

(assert-condition? (request-drop! "/some-path.html"))

(response-put! "/some-path.html" '(200 some-template "some uuid" "some message"))
(assert-equal? '(200 some-template "some uuid" "some message") 
               (response-get! "/some-path.html"))

(report)
