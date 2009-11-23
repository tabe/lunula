#!r6rs

(import (rnrs)
        (lunula input-field)
        (xunit))

(define-record-type record-to-test-add-input-fields
  (fields x y z))

(define select-foo '(select (a "a") (b "b") (c "c")))

(add-input-fields record-to-test-add-input-fields
  (#f
   (text "this is y.")
   select-foo))

(assert-equal? `(#f text ,select-foo) (input-types (record-type-descriptor record-to-test-add-input-fields)))
(assert-equal? '(#f "this is y." #f) (input-descriptions (record-type-descriptor record-to-test-add-input-fields)))

(report)
