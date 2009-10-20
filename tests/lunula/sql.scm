#!r6rs

(import (rnrs)
        (only (lunula persistent-record) define-persistent-record-type id-set! persistent-protocol)
        (lunula sql)
        (xunit))

(define-syntax assert-call-with-tuple
  (syntax-rules ()
    ((_ (c-tuple query) arg0 arg1 arg2 arg3)
     (call-with-tuple
      arg0 arg1 arg2 arg3
      (lambda (x y)
        (assert-equal? 'c-tuple x)
        (assert-string=? query y))))))

(define-syntax assert-delete-query
  (syntax-rules ()
    ((_ expected arg ...)
     (assert-string=? expected (delete-query arg ...)))))

(define-syntax assert-lookup-query
  (syntax-rules ()
    ((_ expected arg ...)
     (assert-string=? expected (lookup-query arg ...)))))

(define-syntax assert-insert-query
  (syntax-rules ()
    ((_ expected arg ...)
     (assert-string=? expected (insert-query arg ...)))))

(define-syntax assert-update-query
  (syntax-rules ()
    ((_ expected arg ...)
     (assert-string=? expected (update-query arg ...)))))

(define-persistent-record-type foo
  (fields x y)
  (protocol
   (persistent-protocol
    (lambda (p)
      (lambda (x y)
        (p x y))))))

(define-persistent-record-type bar
  (fields foo-id y z)
  (protocol
   (persistent-protocol
    (lambda (p)
      (lambda (foo-id y z)
        (p foo-id y z))))))

(assert-string=? "foo" (record-name->table-name 'foo))
(assert-string=? "a_b_c" (record-name->table-name 'a-b-c))

(assert-call-with-tuple ((("t0_0.id" "t0_0.created_at" "t0_0.updated_at" "t0_0.x" "t0_0.y"))
                         "SELECT t0_0.id, t0_0.created_at, t0_0.updated_at, t0_0.x, t0_0.y FROM foo t0_0")
                        values (foo) () ())
(assert-call-with-tuple ((("t0_0.id" "t0_0.created_at" "t0_0.updated_at" "t0_0.foo_id" "t0_0.y" "t0_0.z")
                          ("t0_1.id" "t0_1.created_at" "t0_1.updated_at" "t0_1.x" "t0_1.y"))
                         "SELECT t0_0.id, t0_0.created_at, t0_0.updated_at, t0_0.foo_id, t0_0.y, t0_0.z, t0_1.id, t0_1.created_at, t0_1.updated_at, t0_1.x, t0_1.y FROM bar t0_0 JOIN foo t0_1 ON t0_1.id = t0_0.foo_id")
                        values (bar (foo bar)) () ())
(assert-call-with-tuple ((("t0_0.id" "t0_0.created_at" "t0_0.updated_at" "t0_0.foo_id" "t0_0.y" "t0_0.z")
                          ("t0_1.id" "t0_1.created_at" "t0_1.updated_at" "t0_1.x" "t0_1.y"))
                         "SELECT t0_0.id, t0_0.created_at, t0_0.updated_at, t0_0.foo_id, t0_0.y, t0_0.z, t0_1.id, t0_1.created_at, t0_1.updated_at, t0_1.x, t0_1.y FROM bar t0_0 JOIN foo t0_1 ON t0_1.id = t0_0.foo_id WHERE t0_1.y = '200' AND t0_0.y = '100'")
                        values (bar (foo bar)) ((bar (y 100)) (foo (y 200))) ())
(assert-call-with-tuple ((("t0_0.id" "t0_0.created_at" "t0_0.updated_at" "t0_0.foo_id" "t0_0.y" "t0_0.z")
                          ("t0_1.id" "t0_1.created_at" "t0_1.updated_at" "t0_1.x" "t0_1.y"))
                         "SELECT t0_0.id, t0_0.created_at, t0_0.updated_at, t0_0.foo_id, t0_0.y, t0_0.z, t0_1.id, t0_1.created_at, t0_1.updated_at, t0_1.x, t0_1.y FROM bar t0_0 JOIN foo t0_1 ON t0_1.id = t0_0.foo_id WHERE t0_1.y = '200' AND t0_0.y = '100' ORDER BY t0_1.id desc LIMIT 10 OFFSET 5")
                        values (bar (foo bar)) ((bar (y 100)) (foo (y 200))) ((order-by (foo (id desc))) (offset 5) (limit 10)))
(assert-call-with-tuple ((("t0_0.id" "t0_0.created_at" "t0_0.updated_at" "t0_0.foo_id" "t0_0.y" "t0_0.z"))
                         "SELECT t0_0.id, t0_0.created_at, t0_0.updated_at, t0_0.foo_id, t0_0.y, t0_0.z FROM bar t0_0 WHERE EXISTS (SELECT 1 FROM foo t1_0 WHERE t1_0.z = 'param z' AND t0_0.foo_id = t1_0.id) AND t0_0.x = 'param x' ORDER BY t0_0.y asc")
                        values (bar) ((bar (x "param x")) (exists (foo) ((bar (foo)) (foo (z "param z"))))) ((order-by (bar (y asc)))))

(let ((f (make-foo "scheme" "r6rs")))
  (id-set! f 100)
  (assert-delete-query "DELETE FROM xxx WHERE id = '100'" "xxx" f)
  (id-set! f 200)
  (assert-delete-query "DELETE FROM baz_baz WHERE id = '200'" "baz_baz" f))

(assert-lookup-query "SELECT id, created_at, updated_at FROM abc"
                     values '#() "abc" ())
(assert-lookup-query "SELECT id, created_at, updated_at FROM abc WHERE x = '100'"
                     values '#() "abc" ((x 100)))
(assert-lookup-query "SELECT id, created_at, updated_at FROM abc WHERE x = '100' AND y = 'let me see ...'"
                     values '#() "abc" ((x 100) (y "let me see ...")))
(assert-lookup-query "SELECT id, created_at, updated_at, c0, c1 FROM abc WHERE x = '100' AND y = 'let me see ...'"
                     values '#(c0 c1) "abc" ((x 100) (y "let me see ...")))

(let ((f (make-foo "scheme" "srfi")))
  (assert-insert-query "INSERT INTO quux (created_at, updated_at) VALUES (current_timestamp(), current_timestamp())"
                       values (record-type-descriptor foo) '#() "quux" f)
  (assert-insert-query "INSERT INTO quux (created_at, updated_at, c1, c0) VALUES (current_timestamp(), current_timestamp(), 'srfi', 'scheme')"
                       values (record-type-descriptor foo) '#(c0 c1) "quux" f))

(let ((f (make-foo "scheme" "ypsilon")))
  (id-set! f 7)
  (assert-update-query "UPDATE lunula SET updated_at = current_timestamp() WHERE id = '7'"
                       values (record-type-descriptor foo) '#() "lunula" f)
  (assert-update-query "UPDATE lunula SET updated_at = current_timestamp(), c1 = 'ypsilon', c0 = 'scheme' WHERE id = '7'"
                       values (record-type-descriptor foo) '#(c0 c1) "lunula" f))

(report)
