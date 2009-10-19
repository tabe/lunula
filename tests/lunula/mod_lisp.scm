#!r6rs

(import (rnrs)
        (lunula mod_lisp)
        (xunit))

(define-syntax assert-get-header
  (syntax-rules ()
    ((_ expected str)
     (assert-equal?
      expected
      (call-with-port (open-bytevector-input-port (string->utf8 str)) get-header)))))

(define-syntax assert-premature-end-of-header
  (syntax-rules ()
    ((_ str)
     (assert-raise premature-end-of-header?
       (call-with-port (open-bytevector-input-port (string->utf8 str)) get-header)))))

(define-syntax assert-put-header
  (syntax-rules ()
    ((_ expected header)
     (assert-string=?
      expected
      (call-with-string-output-port
       (lambda (port)
         (put-header port header)))))))

(assert-get-header '()
                   "end\n")
(assert-get-header '(("a" "Alpha")
                     ("b" "Beta"))
                   "a\nAlpha\nb\nBeta\nend\n")

(assert-premature-end-of-header "")
(assert-premature-end-of-header "end")
(assert-premature-end-of-header "a\nb\nc\nend\n")

(assert-put-header "end\n"
                   '())
(assert-put-header "a\nAlpha\nb\nBeta\nend\n"
                   '((a . Alpha)
                     (b . Beta)))

(report)
