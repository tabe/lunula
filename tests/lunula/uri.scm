#!r6rs

(import (rnrs)
        (lunula uri)
        (xunit))

(define-syntax assert-url->fragment
  (syntax-rules ()
    ((_ expected url)
     (assert-equal? expected (url->fragment url)))))

(define-syntax assert-url->parameter
  (syntax-rules ()
    ((_ expected url)
     (assert-equal? expected (url->parameter url)))))

(define-syntax assert-url->path
  (syntax-rules ()
    ((_ expected url)
     (assert-equal? expected (url->path url)))))

(assert-url->fragment #f "")
(assert-url->fragment #f "/path/example")
(assert-url->fragment #f "/path/example#")
(assert-url->fragment "abc" "/path/example#abc")
(assert-url->fragment "abc" "/path/example#abc?def")
(assert-url->fragment "def" "/path/example?abc#def")

(assert-url->parameter #f "")
(assert-url->parameter #f "/path/example")
(assert-url->parameter #f "/path/example?")
(assert-url->parameter "abc" "/path/example?abc")
(assert-url->parameter "def" "/path/example#abc?def")
(assert-url->parameter "abc" "/path/example?abc#def")

(assert-url->path "" "")
(assert-url->path "/path/example" "/path/example")
(assert-url->path "/path/example" "/path/example?abc")
(assert-url->path "/path/example" "/path/example#abc")
(assert-url->path "/path/example" "/path/example?abc#def")
(assert-url->path "/path/example" "/path/example#abc?def")

(report)
