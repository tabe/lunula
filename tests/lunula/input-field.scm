#!r6rs

(import (rnrs)
        (lunula input-field)
        (xunit))

(define-record-type record-to-test-add-input-fields
  (fields x y z z1))

(define select-foo '(select (a "a") (b "b") (c "c")))

(define radio-bar `(procedure ,(lambda () '(radio (a "A" #t) (b "B" #f) (c "C" #f)))))

(add-input-fields record-to-test-add-input-fields
  (#f
   (text "this is y.")
   select-foo
   radio-bar))

(assert-equal? `(#f text ,select-foo ,radio-bar) (input-types (record-type-descriptor record-to-test-add-input-fields)))
(assert-equal? '(#f "this is y." #f #f) (input-descriptions (record-type-descriptor record-to-test-add-input-fields)))

(assert-equal? '(#\< input (#\space type "='" text "'") (#\space name "='" "y" "'") (#\space value "='" "..." "'") " />")
               (input-field 'text "y" "..."))
(assert-equal? '(#\< select (#\space name "='" "z" "'") #\>
                 ((#\< option (#\space value "='" a "'") () #\> "a" "</" option "\n>") (#\< option (#\space value "='" b "'") (#\space selected) #\> "b" "</" option "\n>") (#\< option (#\space value "='" c "'") () #\> "c" "</" option "\n>"))
                 "</" select "\n>")
               (input-field select-foo "z" "b"))
(assert-list? (input-field radio-bar "z1" "b"))

(report)
