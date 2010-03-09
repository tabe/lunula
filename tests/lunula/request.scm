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
                ("method" "GET")
                ("accept-language" "en-US,*;q=0.1"))))
  (assert-= 0 (content-length-of header))
  (assert-string=? "GET" (method-of header))
  (assert-symbol=? 'en (locale-of header)))

(let ((header '(("Content-Length" "100")
                ("method" "POST")
                ("Accept-Language" "da, en-gb; q=0.8, en;q=0.7"))))
  (assert-= 100 (content-length-of header))
  (assert-string=? "POST" (method-of header))
  (assert-boolean=? #f (locale-of header)))

(assert-raise missing-url?
  (fragment-of '()))

(assert-equal? "fragment"
               (fragment-of '(("url" "/?#fragment"))))

(assert-raise missing-url?
  (parameter-of '()))

(assert-equal? "xxx"
               (parameter-of '(("url" "/?xxx"))))

(assert-raise missing-url?
  (path-of '()))

(assert-equal? "/path/to/somewhere"
               (path-of '(("url" "/path/to/somewhere?xxx"))))

(report)
