(library (lunula mysql)
  (export *mysql*
          close
          connect
          destroy
          execute
          lookup
          lookup-all
          save)
  (import (rnrs)
          (only (core) make-parameter)
          (only (srfi :1) list-index iota)
          (only (srfi :13) string-prefix-ci?)
          (only (srfi :48) format)
          (ypsilon ffi)
          (ypsilon mysql)
          (lunula string)
          (prefix (only (lunula log) info) log:)
          (except (lunula persistent-record) define-persistent-record-type))

  (define (record-name->table-name s)
    (string-underscore (symbol->string s)))

  (define (field-name->column-name s)
    (string-underscore (symbol->string s)))

  (define NULL 0)

  (define *mysql* (make-parameter #f))

  (define (connect host user passwd database)
    (*mysql* (mysql_init NULL))
    (if (zero? (mysql_real_connect (*mysql*) host user passwd database 0 NULL 0))
        (mysql_error (*mysql*))
        #t))

  (define (close)
    (mysql_close (*mysql*)))

  (define (escape x)
    (cond ((integer? x) x)
          ((string? x)
           (let* ((bv (string->utf8 x))
                  (len (bytevector-length bv))
                  (dst (make-bytevector (+ (* 2 len) 1))))
             (mysql_real_escape_string (*mysql*) dst x len)
             (utf8->string dst)))
          (else (escape (format "~a" x)))))

  (define (lookup-where names table where)
    (string-append
     "SELECT "
     (fold-left (lambda (x y) (string-append x ", " y))
                "id, created_at, updated_at"
                (map field-name->column-name (vector->list names)))
     (if where
         (format " FROM ~a WHERE ~a" table where)
         (format " FROM ~a" table))))

  (define (lookup-query/id names table id)
    (lookup-where names table (format "id = '~d'" id)))

  (define-syntax lookup-query
    (syntax-rules ()
      ((_ names table ((c v) ...))
       (lookup-where names
                     table
                     (fold-left (lambda (x y)
                                  (let ((name (field-name->column-name (car y))))
                                    (if x
                                        (format "~a AND ~a = '~a'" x name (escape (cadr y)))
                                        (format "~a = '~a'" name (escape (cadr y))))))
                                #f
                                `((c ,v) ...))))
      ((_ names table param)
       (let ((x param))
         (cond ((integer? x)
                (lookup-query/id names table x))
               ((string? x)
                (lookup-where names table x))
               (else
                (raise x)))))))

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

  (define (execute query)
    (log:info "MySQL> ~a" query)
    (let ((r (mysql_query (*mysql*) query)))
      (unless (zero? r) (log:info "MySQL! ~a" (mysql_error (*mysql*))))
      r))

  (define (fields->persistent-record constructor fields)
    (let ((record (apply constructor (cdddr fields))))
      (id-set! record (string->id (car fields)))
      (created-at-set! record (cadr fields))
      (updated-at-set! record (caddr fields))
      record))

  (define (rtd->columns rtd)
    (let ((names (record-type-field-names rtd)))
      (cons* "id"
             "created_at"
             "updated_at"
             (map field-name->column-name (vector->list names)))))

  (define-syntax ->t
    (syntax-rules ()
      ((_ depth record-name0 record-name1 ...)
       (lambda (name)
         (cond ((list-index
                 (lambda (x) (eq? x name))
                 '(record-name0 record-name1 ...))
                => (lambda (i) (format "t~d_~d" depth i)))
               (else #f))))))

  (define-syntax column-tuple
    (syntax-rules ()
      ((_ () proc)
       '())
      ((_ (record-name0 record-name1 ...) proc)
       (cons (map
              (lambda (c) (format "~a.~a" (proc 'record-name0) c))
              (rtd->columns (record-type-descriptor record-name0)))
             (column-tuple (record-name1 ...) proc)))))

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

  (define (equality t column-name value)
    (let ((left (format "~a.~a" t column-name)))
      (if (boolean? value)
          (if value
              (string-append left " IS NOT NULL")
              (string-append left " IS NULL"))
          (format "~a = '~a'" left (escape value)))))

  (define-syntax exists-clause
    (syntax-rules ()
      ((_ depth (record-name (reference foreign) ...) param proc)
       (let* ((depth+1 (+ depth 1))
              (name->t (lambda (x)
                         (or ((->t depth+1 record-name reference ...) x)
                             (proc x))))
              (condition (where depth+1 param name->t)))
         (format "EXISTS (SELECT 1 FROM ~a~a)"
                 (fold-left
                  (lambda (s ref key)
                    (let ((tt (record-name->table-name ref))
                          (ti (name->t ref))
                          (tk (name->t key)))
                      (format "~a JOIN ~a ~a ON ~a.id = ~a.~a_id"
                              s tt ti ti tk tt)))
                  (format "~a ~a" (record-name->table-name 'record-name) (name->t 'record-name))
                  '(reference ...)
                  '(foreign ...))
                 (if (string? condition)
                     (string-append " WHERE " condition)
                     ""))))))

  (define-syntax where
    (syntax-rules (exists)
      ((_ depth () name->t)
       #f)
      ((_ depth ((exists x param) e ...) name->t)
       (let ((s (where depth (e ...) name->t))
             (clause (exists-clause depth x param name->t)))
         (if (string? s)
             (string-append s " AND " clause)
             clause)))
      ((_ depth ((record-name (reference)) e ...) name->t)
       (let ((s (where depth (e ...) name->t))
             (clause (format "~a.~a_id = ~a.id"
                             (name->t 'record-name)
                             (record-name->table-name 'reference)
                             (name->t 'reference))))
         (if (string? s)
             (string-append s " AND " clause)
             clause)))
      ((_ depth ((record-name (field-name value) ...) e ...) name->t)
       (fold-left
        (lambda (s clause)
          (if (string? s)
              (string-append s " AND " clause)
              clause))
        (where depth (e ...) name->t)
        (list (equality (name->t 'record-name)
                        (field-name->column-name 'field-name)
                        value)
              ...)))))

  (define-syntax epilog
    (syntax-rules (order-by offset limit)
      ((_ () name->t)
       "")
      ((_ ((order-by (record-name (field-name dir) ...)) e ...) name->t)
       (string-append
        " ORDER BY "
        (fold-left
         (lambda (s clause)
           (if (string? s)
               (string-append s ", " clause)
               clause))
         #f
         (list (format "~a.~a ~a"
                       (name->t 'record-name)
                       (field-name->column-name 'field-name)
                       'dir)
               ...))
        (epilog (e ...) name->t)))
      ((_ ((offset val) e ...) name->t)
       (format "~a OFFSET ~a" (epilog (e ...) name->t) val))
      ((_ ((limit val) e ...) name->t)
       (format "~a LIMIT ~a" (epilog (e ...) name->t) val))))

  (define-syntax call-with-tuple
    (syntax-rules ()
      ((_ (record-name (reference foreign) ...) param rest cont)
       (let* ((name->t (->t 0 record-name reference ...))
              (condition (where 0 param name->t))
              (tail (epilog rest name->t))
              (c-tuple (column-tuple (record-name reference ...) name->t))
              (query (format "SELECT ~a FROM ~a~a~a"
                             (fold-left
                              (lambda (s column)
                                (if (string? s)
                                    (string-append s ", " column)
                                    column))
                              #f
                              (apply append c-tuple))
                             (fold-left
                              (lambda (s ref key)
                                (let ((tt (record-name->table-name ref))
                                      (ti (name->t ref))
                                      (tk (name->t key)))
                                  (format "~a JOIN ~a ~a ON ~a.id = ~a.~a_id"
                                          s tt ti ti tk tt)))
                              (format "~a ~a" (record-name->table-name 'record-name) (name->t 'record-name))
                              '(reference ...)
                              '(foreign ...))
                             (if (string? condition)
                                 (string-append " WHERE " condition)
                                 "")
                             tail)))
         (cont c-tuple query)))))

  (define-syntax lookup
    (syntax-rules ()
      ((_ (record-name (reference foreign) ...) param rest)
       (call-with-tuple
        (record-name (reference foreign) ...)
        param
        ()
        (lambda (c-tuple query)
          (if (not (zero? (execute query)))
              #f
              (let ((result (mysql_store_result (*mysql*))))
                (if (zero? result)
                    #f
                    (let ((row (mysql_fetch_row result)))
                      (and (not (zero? row))
                           (let ((ft (row->field-tuple c-tuple result row)))
                             (mysql_free_result result)
                             (and ft
                                  (field-tuple->persistent-record-tuple (record-name reference ...) ft)))))))))))
      ((_ (record-name (reference foreign) ...) param)
       (lookup (record-name (reference foreign) ...) param ()))
      ((_ record-name param rest)
       (let* ((rtd (record-type-descriptor record-name))
              (c (record-constructor (record-constructor-descriptor record-name)))
              (names (record-type-field-names rtd))
              (table (record-name->table-name (record-type-name rtd)))
              (query (string-append (lookup-query names table param) rest)))
         (if (not (zero? (execute query)))
             #f
             (let ((result (mysql_store_result (*mysql*))))
               (if (zero? result)
                   #f
                   (let ((row (mysql_fetch_row result)))
                     (and (not (zero? row))
                          (let ((fields (row->fields names result row)))
                            (mysql_free_result result)
                            (and fields
                                 (fields->persistent-record c fields))))))))))
      ((_ record-name param)
       (lookup record-name param ""))))

  (define-syntax lookup-all
    (syntax-rules ()
      ((_ (record-name (reference foreign) ...) param rest)
       (call-with-tuple
        (record-name (reference foreign) ...)
        param
        rest
        (lambda (c-tuple query)
          (if (not (zero? (execute query)))
              #f
              (let ((result (mysql_store_result (*mysql*))))
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
                            (else (loop (cons #f ls) (mysql_fetch_row result)))))))))))
      ((_ (record-name (reference foreign) ...) param)
       (lookup-all (record-name (reference foreign) ...) param ()))
      ((_ record-name param rest)
       (let* ((rtd (record-type-descriptor record-name))
              (c (record-constructor (record-constructor-descriptor record-name)))
              (names (record-type-field-names rtd))
              (table (record-name->table-name (record-type-name rtd)))
              (query (string-append (lookup-query names table param) rest)))
         (if (not (zero? (execute query)))
             #f
             (let ((result (mysql_store_result (*mysql*))))
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
                                    (mysql_fetch_row result)))))))))))
      ((_ record-name param)
       (lookup-all record-name param ""))))

  (define (call/pv rtd names record proc) ; call-with-proper-values
    (let ((ns (vector->list names)))
      (apply proc
             (fold-left (lambda (couple name value)
                          (if value
                              `(,(cons (field-name->column-name name) (car couple))
                                ,(cons value (cadr couple)))
                              couple))
                        '(() ())
                        ns
                        (map (lambda (i) ((record-accessor rtd i) record)) (iota (length ns)))))))

  (define (update-query rtd names table record)
    (call/pv
     rtd names record
     (lambda (cols vals)
       (format "UPDATE ~a SET ~a WHERE id = '~d'"
               table
               (fold-left (lambda (x name value)
                            (format "~a, ~a = '~a'" x name (escape value)))
                          "updated_at = current_timestamp()"
                          cols
                          vals)
               (id-of record)))))

  (define (insert-query rtd names table record)
    (call/pv
     rtd names record
     (lambda (cols vals)
       (format "INSERT INTO ~a (~a) VALUES (~a)"
               table
               (fold-left (lambda (s name) (string-append s ", " name))
                          "created_at, updated_at"
                          cols)
               (fold-left (lambda (s value) (format "~a, '~a'" s (escape value)))
                          "current_timestamp(), current_timestamp()"
                          vals)))))

  (define (save record)
    (let* ((rtd (record-rtd record))
           (names (record-type-field-names rtd))
           (table (record-name->table-name (record-type-name rtd)))
           (id (id-of record)))
      (if (integer? id)
          (let ((query (string-append (lookup-query names table id) " FOR UPDATE")))
            (and (zero? (execute query))
                 (let ((result (mysql_store_result (*mysql*))))
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
                              (let ((query (insert-query rtd names table record)))
                                (and (zero? (execute query))
                                     (< 0 (mysql_affected_rows (*mysql*)))))
                              (let ((query (update-query rtd names table record)))
                                (and (zero? (execute query))
                                     (mysql_affected_rows (*mysql*)))))))))))
          (let ((query (insert-query rtd names table record)))
            (cond ((and (zero? (execute query))
                        (< 0 (mysql_affected_rows (*mysql*))))
                   (id-set! record (mysql_insert_id (*mysql*)))
                   #t)
                  (else
                   #f))))))

  (define (delete-query/id table id)
    (format "DELETE FROM ~a WHERE id = '~d'" table id))

  (define (delete-query table record)
    (delete-query/id table (id-of record)))

  (define-syntax destroy
    (syntax-rules ()
      ((_ record)
       (let* ((table (record-name->table-name (record-type-name (record-rtd record))))
              (query (delete-query table record)))
         (and (zero? (execute query))
              (mysql_affected_rows (*mysql*)))))
      ((_ record-name id)
       (and (zero? (execute (delete-query/id (record-name->table-name 'record-name) id)))
            (mysql_affected_rows (*mysql*))))))
)
