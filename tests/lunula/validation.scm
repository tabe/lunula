#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (only (lunula gettext) gettext)
        (lunula validation)
        (xunit))

(define-predicate-validator validate-integer
  not-an-integer
  integer?)

(define-condition-validator validate-x
  ((error-x error?))
  (lambda (x)
    (or x
        (error 'validate-x "x")))
  validate-integer)

(define-string-length-validator validate-2/5
  (is-blank too-short too-long)
  (2 5))

(define-string-length-validator validate-1/5
  (is-blank too-long)
  (5))

(define-string-length-validator validate-0/5
  (too-long)
  (5))

(define-composite-validator validate-2/5-1/5-0/5
  ((0) validate-2/5)
  ((1) validate-1/5)
  ((2) validate-0/5))

(define-predicate-validator validate-even not-even even?)
(define-predicate-validator validate-odd not-odd odd?)

(define-composite-validator validate-even-odd-even
  (car validate-even)
  (cadr validate-odd)
  (caddr validate-even))

(gettext
 (not-an-integer (en "Not an integer"))
 )

(assert-= 1 (guide (validate-integer 1)
              (lambda _ #f)
              values))
(assert-boolean=? #f (guide (validate-integer "1")
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'not-an-integer)
                         (assert-null? (hashtable-ref ht 'not-an-integer #f))
                         #f)
                       values))

(assert-boolean=? #f (guide (validate-x #f)
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'error-x)
                         (assert-error? (hashtable-ref ht 'error-x #f))
                         #f)
                       (lambda (x) #t)))
(assert-boolean=? #f (guide (validate-x #t)
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'not-an-integer)
                         (assert-null? (hashtable-ref ht 'not-an-integer #f))
                         #f)
                       (lambda (x) #t)))
(assert-= 1 (guide (validate-x 1)
              (lambda _ #f)
              values))

(assert-boolean=? #f (guide (validate-2/5 "")
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'is-blank)
                         (assert-null? (hashtable-ref ht 'is-blank #f))
                         #f)
                       (lambda _ #t)))
(assert-boolean=? #f (guide (validate-2/5 "1")
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'too-short)
                         (assert-null? (hashtable-ref ht 'too-short #f))
                         #f)
                       (lambda _ #t)))
(assert-= 4 (guide (validate-2/5 "1234")
              (lambda _ #f)
              values))
(assert-boolean=? #f (guide (validate-2/5 "123456")
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'too-long)
                         (assert-null? (hashtable-ref ht 'too-long #f))
                         #f)
                       (lambda _ #t)))

(assert-boolean=? #f (guide (validate-1/5 "")
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'is-blank)
                         (assert-null? (hashtable-ref ht 'is-blank #f))
                         #f)
                       (lambda _ #t)))
(assert-= 3 (guide (validate-1/5 "123")
              (lambda _ #f)
              values))
(assert-boolean=? #f (guide (validate-1/5 "123456")
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'too-long)
                         (assert-null? (hashtable-ref ht 'too-long #f))
                         #f)
                       (lambda _ #t)))

(assert-= 0 (guide (validate-0/5 "")
              (lambda _ #f)
              values))
(assert-= 3 (guide (validate-0/5 "123")
              (lambda _ #f)
              values))
(assert-boolean=? #f (guide (validate-0/5 "123456")
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'too-long)
                         (assert-null? (hashtable-ref ht 'too-long #f))
                         #f)
                       (lambda _ #t)))

(assert-boolean=? #f (guide (validate-2/5-1/5-0/5 '("a" "" "bcdefg"))
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'is-blank)
                         (assert-null? (hashtable-ref ht 'is-blank #f))
                         (assert-hashtable-contains? ht 'too-short)
                         (assert-null? (hashtable-ref ht 'too-short #f))
                         (assert-hashtable-contains? ht 'too-long)
                         (assert-null? (hashtable-ref ht 'too-long #f))
                         #f)
                       (lambda _ #t)))
(assert-= 1 (guide (validate-2/5-1/5-0/5 '("abcd" "ef" "g"))
              (lambda _ #f)
              values))

(assert-boolean=? #f (guide (validate-even-odd-even '(3 3 3))
                       (lambda (ht)
                         (assert-hashtable-contains? ht 'not-even)
                         (assert-null? (hashtable-ref ht 'not-even #f))
                         #f)
                       (lambda _ #t)))
(assert-= 2 (guide (validate-even-odd-even '(0 1 2))
              (lambda _ #f)
              values))

(report)
