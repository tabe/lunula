(library (lunula mysql)
  (export *mysql*
          call-with-mysql
          close
          connect
          destroy
          execute
          lookup
          lookup-all
          save)
  (import (rnrs)
          (only (srfi :1) iota)
          (only (srfi :48) format)
          (only (ypsilon concurrent) make-mailbox recv send)
          (ypsilon ffi)
          (ypsilon mysql)
          (only (lunula string))
          (prefix (only (lunula log) info) log:)
          (only (lunula persistent-record) created-at-set! id-of id-set! maybe-id updated-at-set!)
          (only (lunula sql) call-with-tuple delete-query insert-query lookup-query record-name->table-name update-query))

  (define NULL 0)

  (define *mysql* (make-mailbox))

  (define (connect host user passwd database)
    (let ((mysql (mysql_init NULL)))
      (cond ((zero? (mysql_real_connect mysql host user passwd database 0 NULL 0))
             (mysql_error mysql))
            (else
             (send *mysql* mysql)
             #t))))

  (define (close)
    (mysql_close (recv *mysql*)))

  (define (escape/mysql mysql x)
    (cond ((integer? x) x)
          ((string? x)
           (let* ((bv (string->utf8 x))
                  (len (bytevector-length bv))
                  (dst (make-bytevector (+ (* 2 len) 1))))
             (mysql_real_escape_string mysql dst x len)
             (utf8->string dst)))
          (else (escape/mysql mysql (format "~a" x)))))

  (define (row->fields names result row)
    (let ((num (mysql_num_fields result))
          (lengths (mysql_fetch_lengths result)))
      (and (= num (+ 3 (vector-length names)))
           (let lp ((i 0)
                    (fields '()))
             (if (= i num)
                 (reverse fields)
                 (let* ((f (c-void*-ref (+ row (* i sizeof:void*))))
                        (v (if (zero? f)
                               #f
                               (utf8->string (make-bytevector-mapping f (c-unsigned-int-ref (+ lengths (* i sizeof:int))))))))
                   (lp (+ i 1) (cons v fields))))))))

  (define-syntax call-with-mysql
    (syntax-rules ()
      ((_ proc)
       (let ((mysql (recv *mysql*)))
         (dynamic-wind
             (lambda () #f)
             (lambda () (proc mysql))
             (lambda () (send *mysql* mysql)))))))

  (define-syntax execute
    (syntax-rules ()
      ((_ mysql query)
       (begin
         (log:info "MySQL> ~a" query)
         (let ((r (mysql_query mysql query)))
           (unless (zero? r) (log:info "MySQL! ~a" (mysql_error mysql)))
           r)))
      ((_ query)
       (call-with-mysql (lambda (mysql) (execute mysql query))))))

  (define (fields->persistent-record constructor fields)
    (let ((record (apply constructor (cdddr fields))))
      (id-set! record (maybe-id (car fields)))
      (created-at-set! record (cadr fields))
      (updated-at-set! record (caddr fields))
      record))

  (define (row->field-tuple c-tuple result row)
    (let ((num (mysql_num_fields result))
          (lengths (mysql_fetch_lengths result)))
      (and (= num (apply + (map length c-tuple)))
           (let ((len (length c-tuple)))
             (let loop ((k 0)
                        (n 0)
                        (f-tuple '()))
               (if (= k len)
                   (reverse f-tuple)
                   (let ((l (length (list-ref c-tuple k))))
                     (let lp ((i 0)
                              (fields '()))
                       (if (= i l)
                           (loop (+ k 1) (+ n i) (cons (reverse fields) f-tuple))
                           (let* ((f (c-void*-ref (+ row (* (+ n i) sizeof:void*))))
                                  (v (if (zero? f)
                                         #f
                                         (utf8->string (make-bytevector-mapping f (c-unsigned-int-ref (+ lengths (* (+ n i) sizeof:int))))))))
                             (lp (+ i 1) (cons v fields))))))))))))

  (define-syntax field-tuple->persistent-record-tuple
    (syntax-rules ()
      ((_ (record-name0 record-name1 ...) field-tuple)
       (map
        (lambda (record-name fields)
          (fields->persistent-record
           (record-constructor (record-constructor-descriptor record-name))
           fields))
        (list record-name0 record-name1 ...)
        field-tuple))))

  (define-syntax lookup
    (syntax-rules ()
      ((_ (record-name (reference foreign) ...) param rest)
       (call-with-mysql
        (lambda (mysql)
          (call-with-tuple
           (lambda (x) (escape/mysql mysql x))
           (record-name (reference foreign) ...)
           param
           ()
           (lambda (c-tuple query)
             (if (not (zero? (execute mysql query)))
                 #f
                 (let ((result (mysql_store_result mysql)))
                   (if (zero? result)
                       #f
                       (let ((row (mysql_fetch_row result)))
                         (and (not (zero? row))
                              (let ((ft (row->field-tuple c-tuple result row)))
                                (mysql_free_result result)
                                (and ft
                                     (field-tuple->persistent-record-tuple (record-name reference ...) ft)))))))))))))
      ((_ (record-name (reference foreign) ...) param)
       (lookup (record-name (reference foreign) ...) param ()))
      ((_ record-name param rest)
       (call-with-mysql
        (lambda (mysql)
          (let* ((escape (lambda (x) (escape/mysql mysql x)))
                 (rtd (record-type-descriptor record-name))
                 (c (record-constructor (record-constructor-descriptor record-name)))
                 (names (record-type-field-names rtd))
                 (table (record-name->table-name (record-type-name rtd)))
                 (query (string-append (lookup-query escape names table param) rest)))
            (if (not (zero? (execute mysql query)))
                #f
                (let ((result (mysql_store_result mysql)))
                  (if (zero? result)
                      #f
                      (let ((row (mysql_fetch_row result)))
                        (and (not (zero? row))
                             (let ((fields (row->fields names result row)))
                               (mysql_free_result result)
                               (and fields
                                    (fields->persistent-record c fields))))))))))))
      ((_ record-name param)
       (lookup record-name param ""))))

  (define-syntax lookup-all
    (syntax-rules ()
      ((_ (record-name (reference foreign) ...) param rest)
       (call-with-mysql
        (lambda (mysql)
          (call-with-tuple
           (lambda (x) (escape/mysql mysql x))
           (record-name (reference foreign) ...)
           param
           rest
           (lambda (c-tuple query)
             (if (not (zero? (execute mysql query)))
                 #f
                 (let ((result (mysql_store_result mysql)))
                   (if (zero? result)
                       '()
                       (let loop ((ls '())
                                  (row (mysql_fetch_row result)))
                         (cond ((zero? row)
                                (mysql_free_result result)
                                (reverse ls))
                               ((row->field-tuple c-tuple result row)
                                => (lambda (ft)
                                     (let ((rt (field-tuple->persistent-record-tuple (record-name reference ...) ft)))
                                       (loop (cons rt ls) (mysql_fetch_row result)))))
                               (else (loop (cons #f ls) (mysql_fetch_row result)))))))))))))
      ((_ (record-name (reference foreign) ...) param)
       (lookup-all (record-name (reference foreign) ...) param ()))
      ((_ record-name param rest)
       (call-with-mysql
        (lambda (mysql)
          (let* ((escape (lambda (x) (escape/mysql mysql x)))
                 (rtd (record-type-descriptor record-name))
                 (c (record-constructor (record-constructor-descriptor record-name)))
                 (names (record-type-field-names rtd))
                 (table (record-name->table-name (record-type-name rtd)))
                 (query (string-append (lookup-query escape names table param) rest)))
            (if (not (zero? (execute mysql query)))
                #f
                (let ((result (mysql_store_result mysql)))
                  (if (zero? result)
                      '()
                      (let loop ((ls '())
                                 (row (mysql_fetch_row result)))
                        (cond ((zero? row)
                               (mysql_free_result result)
                               (reverse ls))
                              (else
                               (let ((fields (row->fields names result row)))
                                 (loop (cons (and fields (fields->persistent-record c fields)) ls)
                                       (mysql_fetch_row result)))))))))))))
      ((_ record-name param)
       (lookup-all record-name param ""))))

  (define-syntax save
    (syntax-rules ()
      ((_ mysql record)
       (let* ((escape (lambda (x) (escape/mysql mysql x)))
              (rtd (record-rtd record))
              (names (record-type-field-names rtd))
              (table (record-name->table-name (record-type-name rtd)))
              (id (id-of record)))
         (if (integer? id)
             (let ((query (string-append (lookup-query escape names table id) " FOR UPDATE")))
               (and (zero? (execute mysql query))
                    (let ((result (mysql_store_result mysql)))
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
                                                            (cdddr fields)
                                                            (map (lambda (i) ((record-accessor rtd i) record))
                                                                 (iota (vector-length names))))
                                                           (cont 0)))
                                                     (else
                                                      (cont #f)))))))
                                     (lambda ()
                                       (mysql_free_result result)))
                                 (let ((query (insert-query escape rtd names table record)))
                                   (and (zero? (execute mysql query))
                                        (< 0 (mysql_affected_rows mysql))))
                                 (let ((query (update-query escape rtd names table record)))
                                   (and (zero? (execute mysql query))
                                        (mysql_affected_rows mysql))))))))))
             (let ((query (insert-query escape rtd names table record)))
               (cond ((and (zero? (execute mysql query))
                           (< 0 (mysql_affected_rows mysql)))
                      (id-set! record (mysql_insert_id mysql))
                      #t)
                     (else
                      #f))))))
      ((_ record)
       (call-with-mysql (lambda (mysql) (save mysql record))))))

  (define-syntax destroy
    (syntax-rules ()
      ((_ mysql record)
       (let* ((table (record-name->table-name (record-type-name (record-rtd record))))
              (query (delete-query table record)))
         (and (zero? (execute mysql query))
              (mysql_affected_rows mysql))))
      ((_ record)
       (call-with-mysql (lambda (mysql) (destroy mysql record))))))

)
