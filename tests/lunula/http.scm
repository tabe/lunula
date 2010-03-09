#!r6rs

(import (rnrs (6))
        (lunula http)
        (xunit))

(define-syntax assert-accept-language
  (syntax-rules ()
    ((_ ls str)
     (begin
       (assert-equal? ls (string->accept-language str))
       (assert-equal? str (accept-language->string ls))))))

(assert-accept-language '() "")
(assert-accept-language '(("*")) "*")
(assert-accept-language '(("da") ("en-gb" "0.8") ("en" "0.7")) "da, en-gb;q=0.8, en;q=0.7")

(report)
