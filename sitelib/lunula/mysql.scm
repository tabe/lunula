(library (lunula mysql)
  (export *mysql*
          close
          connect
          destroy
          lookup
          save)
  (import (rnrs)
          (only (srfi :1) iota)
          (only (srfi :13) string-prefix-ci?)
          (only (srfi :48) format)
          (ypsilon ffi)
          (ypsilon mysql))

  (define NULL 0)

  (define *mysql* (mysql_init NULL))

  (define (connect host user passwd database)
    (if (zero? (mysql_real_connect *mysql* host user passwd database 0 NULL 0))
        (mysql_error *mysql*)
        #t))

  (define (close)
    (mysql_close *mysql*))

  (define (lookup-where names table where)
    (string-append
     "SELECT "
     (fold-left (lambda (x y)
                  (if x (string-append x ", " y) y))
                #f
                (map symbol->string (vector->list names)))
     (format " FROM ~a WHERE ~a" table where)))

  (define (lookup-query/id names table id)
    (lookup-where names table (format "id = '~d'" id)))

  (define (lookup-query/list names table c)
    (lookup-where names
                  table
                  (fold-left (lambda (x y)
                               (if x
                                   (format "~a AND ~a = '~a'" x (car y) (cadr y))
                                   (format "~a = '~a'" (car y) (cadr y))))
                             #f
                             c)))

  (define (lookup-query names table param)
    (cond ((integer? param)
           (lookup-query/id names table param))
          ((list? param)
           (lookup-query/list names table param))
          (else
           (raise param))))

  (define (row->fields names result row)
    (let ((num (mysql_num_fields result))
          (lengths (mysql_fetch_lengths result)))
      (and (= num (vector-length names))
           (let lp ((i 0)
                    (fields '()))
             (if (= i num)
                 (reverse fields)
                 (let* ((f (c-void*-ref (+ row (* i sizeof:void*))))
                        (v (if (zero? f)
                               #f
                               (utf8->string (make-bytevector-mapping f (c-unsigned-int-ref (+ lengths (* i sizeof:int))))))))
                   (lp (+ i 1) (cons v fields))))))))

  (define-syntax lookup
    (syntax-rules ()
      ((_ record-name param rest)
       (let* ((rtd (record-type-descriptor record-name))
              (c (record-constructor (record-constructor-descriptor record-name)))
              (names (record-type-field-names rtd))
              (table (record-type-name rtd))
              (query (string-append (lookup-query names table param) rest)))
         (if (not (zero? (mysql_query *mysql* query)))
             #f
             (let ((result (mysql_store_result *mysql*)))
               (if (zero? result)
                   #f
                   (dynamic-wind
                       (lambda () #f)
                       (lambda ()
                         (let ((row (mysql_fetch_row result)))
                           (and (not (zero? row))
                                (let ((fields (row->fields names result row)))
                                  (and fields (apply c fields))))))
                       (lambda ()
                         (mysql_free_result result))))))))
      ((_ record-name param)
       (lookup record-name param ""))))

  (define (id-of rtd record)
    (let ((id ((record-accessor rtd 0) record)))
      (if (string? id) (string->number id) id)))

  (define (update-query names table record)
    (let ((rtd (record-rtd record))
          (ns (map symbol->string (cdr (vector->list names)))))
      (string-append
       (format "UPDATE ~a SET " table)
       (fold-left (lambda (x name value)
                    (if x
                        (format "~a, ~a = '~a'" x name value)
                        (format "~a = '~a'" name value)))
                  #f
                  ns
                  (map (lambda (i) ((record-accessor rtd i) record)) (iota (length ns) 1)))
       (format " WHERE id = '~d'" (id-of rtd record)))))

  (define (insert-query rtd names table record)
    (define (%insert-query% proc)
      (let ((ns (vector->list names)))
        (string-append
         (format "INSERT INTO ~a (" table)
         (fold-left (lambda (s name) (if s (string-append s ", " name) name))
                    #f
                    (map symbol->string (proc ns)))
         ") VALUES ("
         (fold-left (lambda (s value)
                      (if s
                          (format "~a, '~a'" s value)
                          (format "'~a'" value)))
                    #f
                    (map (lambda (i) ((record-accessor rtd i) record)) (proc (iota (length ns)))))
         ")")))
    (cond ((integer? (id-of rtd record))
           (%insert-query% values))
          (else
           (%insert-query% cdr))))

  (define (save record)
    (let* ((rtd (record-rtd record))
           (names (record-type-field-names rtd))
           (table (record-type-name rtd))
           (id (id-of rtd record)))
      (if (integer? id)
          (let ((query (string-append (lookup-query names table id) " FOR UPDATE")))
            (and (zero? (mysql_query *mysql* query))
                 (let ((result (mysql_store_result *mysql*)))
                   (if (zero? result)
                       #f
                       (call/cc
                        (lambda (cont)
                          (if (dynamic-wind
                                  (lambda () #f)
                                  (lambda ()
                                    (let ((row (mysql_fetch_row result)))
                                      (or (zero? row)
                                          (let ((fields (row->fields names result row)))
                                            (cond ((list? fields)
                                                   (and (for-all
                                                         equal?
                                                         (cdr fields)
                                                         (map (lambda (i) ((record-accessor rtd i) record))
                                                              (iota (- (vector-length names) 1) 1)))
                                                        (cont 0)))
                                                  (else
                                                   (cont #f)))))))
                                  (lambda ()
                                    (mysql_free_result result)))
                              (let ((query (insert-query rtd names table record)))
                                (and (zero? (mysql_query *mysql* query))
                                     (< 0 (mysql_affected_rows *mysql*))))
                              (let ((query (update-query names table record)))
                                (and (zero? (mysql_query *mysql* query))
                                     (mysql_affected_rows *mysql*))))))))))
          (let ((query (insert-query rtd names table record)))
            (and (zero? (mysql_query *mysql* query))
                 (< 0 (mysql_affected_rows *mysql*)))))))

  (define (delete-query table record)
    (format "DELETE FROM ~a WHERE id = '~d'" table (id-of (record-rtd record) record)))

  (define (destroy record)
    (let* ((table (record-type-name (record-rtd record)))
           (query (delete-query table record)))
      (and (zero? (mysql_query *mysql* query))
           (mysql_affected_rows *mysql*))))
                 
)
