#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (lunula mysql)
        (only (lunula configuration) define-configuration)
        (only (lunula persistent-record) define-persistent-record-type persistent-protocol id-of)
        (prefix (only (lunula log) info) log:)
        (xunit))

(define-syntax assert-execute
  (syntax-rules ()
    ((_ query)
     (assert-= 0 (execute query)))))

(define-syntax assert-drop-table-if-exists
  (syntax-rules ()
    ((_ name)
     (assert-execute (string-append "DROP TABLE IF EXISTS " (symbol->string 'name))))))

(define-syntax assert-save
  (syntax-rules ()
    ((_ expected x)
     (if (boolean? expected)
         (assert-boolean=? expected (save x))
         (assert-= expected (save x))))))

(define-syntax assert-destroy
  (syntax-rules ()
    ((_ expected x)
     (assert-= expected (destroy x)))))

(define-persistent-record-type foobar
  (fields (mutable name) (mutable memo))
  (protocol
   (persistent-protocol
    (lambda (p)
      (lambda (x y)
        (p x y))))))

(define-persistent-record-type baz
  (fields name foobar-id)
  (protocol
   (persistent-protocol
    (lambda (p)
      (lambda (x y)
        (p x (if (string? y) (string->number y) y)))))))

(define-persistent-record-type cuux
  (fields name baz-id)
  (protocol
   (persistent-protocol
    (lambda (p)
      (lambda (x y)
        (p x (if (string? y) (string->number y) y)))))))

(define-syntax foobar-is
  (syntax-rules ()
    ((_ id name memo)
     (let ((x (lookup foobar id)))
       (assert-boolean=? #t (foobar? x))
       (assert-= id (id-of a))
       (assert-string=? name (foobar-name x))
       (assert-string=? memo (foobar-memo x))))))

(define a (make-foobar "lol" "________"))

(define-configuration mysql-user)
(define-configuration mysql-password)
(define-configuration mysql-database)

(connect "localhost" mysql-user mysql-password "")

(assert-execute "CREATE DATABASE IF NOT EXISTS lunula")
(assert-execute "USE lunula")
(assert-drop-table-if-exists foobar)
(assert-execute "CREATE TABLE foobar (\
  id INT NOT NULL AUTO_INCREMENT,\
  name VARCHAR(256),\
  memo VARCHAR(256),\
  created_at DATETIME,\
  updated_at DATETIME,\
  PRIMARY KEY (id)\
) DEFAULT CHARSET=UTF8 TYPE=InnoDB")
(assert-drop-table-if-exists baz)
(assert-execute "CREATE TABLE baz (\
  id INT NOT NULL AUTO_INCREMENT,\
  name VARCHAR(256),\
  foobar_id INT NOT NULL,\
  created_at DATETIME,\
  updated_at DATETIME,\
  PRIMARY KEY (id)\
) DEFAULT CHARSET=UTF8 TYPE=InnoDB")
(assert-drop-table-if-exists cuux)
(assert-execute "CREATE TABLE cuux (\
  id INT NOT NULL AUTO_INCREMENT,\
  name VARCHAR(256),\
  baz_id INT NOT NULL,\
  created_at DATETIME,\
  updated_at DATETIME,\
  PRIMARY KEY (id)\
) DEFAULT CHARSET=UTF8 TYPE=InnoDB")

;; save
(assert-save #t a)
(foobar-is (id-of a) "lol" "________")
(foobar-name-set! a #f)
(foobar-memo-set! a "let me see ...")
(assert-save 1 a)
(foobar-is (id-of a) "lol" "let me see ...")
(let ((b (make-baz "b" (id-of a))))
  (assert-save #t b)
  (let ((c (make-cuux "c" (id-of b))))
    (assert-save #t c)))

;; destroy
(assert-execute "BEGIN")
(assert-destroy 1 a)
(assert-destroy 0 a)
(assert-execute "ROLLBACK")

;; lookup
(let ((x (lookup foobar `((name "lol")))))
  (assert (foobar? x)))

;; lookup with foregin references
(let ((tuple (lookup (cuux
                      (baz cuux)
                      (foobar baz))
                     ((cuux (name "c"))))))
  (assert (list? tuple))
  (assert-= 3 (length tuple))
  (assert (cuux? (car tuple)))
  (assert (baz? (cadr tuple)))
  (assert (foobar? (caddr tuple))))
(assert-boolean=? #f (lookup (cuux
                              (baz cuux)
                              (foobar baz))
                             ((cuux (name "c"))
                              (foobar (name "no such name")))))

;; lookup-all
(assert-execute "BEGIN")
(assert-execute "INSERT INTO foobar (name, memo) VALUES ('temporary', 'temporary')")
(let ((len (length (lookup-all foobar '()))))
  (assert-= 2 len))
(assert-execute "DELETE FROM foobar")
(let ((len (length (lookup-all foobar '()))))
  (assert-= 0 len))
(assert-execute "ROLLBACK")

;; lookup-all with foregin references
(assert-execute "BEGIN")
(let ((b (make-baz "ba" (id-of a))))
  (assert-save #t b)
  (let ((c (make-cuux "cu" (id-of b))))
    (assert-save #t c)))
(let ((tuples (lookup-all (cuux
                           (baz cuux)
                           (foobar baz))
                          ((cuux (name #t))))))
  (assert (list? tuples))
  (assert-= 2 (length tuples))
  (for-each
   (lambda (tuple)
     (assert-= 3 (length tuple)))
   tuples))
(let ((tuples (lookup-all (cuux
                           (baz cuux)
                           (foobar baz))
                          ((baz (name #f))))))
  (assert (list? tuples))
  (assert (null? tuples)))
(assert-execute "ROLLBACK")

(close)

(report)
