#!r6rs

(import (rnrs)
        (lunula request)
        (xunit))

(define-syntax assert-content->alist
  (syntax-rules ()
    ((_ expected content)
     (assert-equal? expected (content->alist content)))))

(assert-raise malformed-key-value?
  (content->alist "="))
(assert-raise malformed-key-value?
  (content->alist "x=a=b&"))

(assert-content->alist '() "")
(assert-content->alist '() "&")
(assert-content->alist '() "&&")
(assert-content->alist '((x . "")) "x=&")
(assert-content->alist '((x . "foo bar") (y . "/path/to/there")) "x=foo+bar&y=%2fpath%2fto%2fthere")

(assert-raise invalid-content-length?
  (content-length-of '(("content-length" ""))))

(assert-raise invalid-content-length?
  (content-length-of '(("content-length" "#f"))))

(assert-raise invalid-content-length?
  (content-length-of '(("content-length" "-1"))))

(assert-raise invalid-content-length?
  (content-length-of '(("content-length" "2.5"))))

(assert-raise missing-content-length?
  (content-length-of '()))

(assert-raise missing-method?
  (method-of '()))

(let ((header '(("content-length" "0")
                ("method" "GET"))))
  (assert-= 0 (content-length-of header))
  (assert-string=? "GET" (method-of header)))

(let ((header '(("Content-Length" "100")
                ("method" "POST"))))
  (assert-= 100 (content-length-of header))
  (assert-string=? "POST" (method-of header)))

(report)
