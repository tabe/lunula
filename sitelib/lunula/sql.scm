(library (lunula sql)
  (export call-with-tuple
          delete-query
          insert-query
          lookup-query
          record-name->table-name
          update-query)
  (import (rnrs)
          (only (srfi :1) list-index iota)
          (only (srfi :48) format)
          (only (lunula persistent-record) id-of)
          (only (lunula string) string-underscore))

  (define (field-name->column-name s)
    (string-underscore (symbol->string s)))

  (define (record-name->table-name s)
    (string-underscore (symbol->string s)))

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

  (define-syntax lookup-query
    (syntax-rules ()
      ((_ escape names table ((c v) ...))
       (lookup-where names
                     table
                     (fold-left (lambda (x y)
                                  (let ((name (field-name->column-name (car y))))
                                    (if x
                                        (format "~a AND ~a = '~a'" x name (escape (cadr y)))
                                        (format "~a = '~a'" name (escape (cadr y))))))
                                #f
                                `((c ,v) ...))))
      ((_ escape names table param)
       (let ((x param))
         (cond ((integer? x)
                (lookup-query/id names table x))
               ((string? x)
                (lookup-where names table x))
               (else
                (raise x)))))))

  (define (equality escape t column-name value)
    (let ((left (format "~a.~a" t column-name)))
      (if (boolean? value)
          (if value
              (string-append left " IS NOT NULL")
              (string-append left " IS NULL"))
          (format "~a = '~a'" left (escape value)))))

  (define-syntax join
    (syntax-rules ()
      ((_ name->t record-name (reference foreign) ...)
       (fold-left
        (lambda (s ref key)
          (let ((tt (record-name->table-name ref))
                (ti (name->t ref)))
            (cond ((pair? key)
                   (let ((k (car key)))
                     (format "~a JOIN ~a ~a ON ~a.~a_id = ~a.id"
                             s tt ti ti (record-name->table-name k) (name->t k))))
                  (else
                   (format "~a JOIN ~a ~a ON ~a.id = ~a.~a_id"
                           s tt ti ti (name->t key) tt)))))
        (format "~a ~a" (record-name->table-name 'record-name) (name->t 'record-name))
        '(reference ...)
        '(foreign ...)))))

  (define-syntax exists-clause
    (syntax-rules ()
      ((_ escape depth (record-name (reference foreign) ...) param proc)
       (let* ((depth+1 (+ depth 1))
              (name->t (lambda (x)
                         (or ((->t depth+1 record-name reference ...) x)
                             (proc x))))
              (condition (where escape depth+1 param name->t)))
         (format "EXISTS (SELECT 1 FROM ~a~a)"
                 (join name->t record-name (reference foreign) ...)
                 (if (string? condition)
                     (string-append " WHERE " condition)
                     ""))))))

  (define-syntax where
    (syntax-rules (exists)
      ((_ escape depth () name->t)
       #f)
      ((_ escape depth ((exists x param) e ...) name->t)
       (let ((s (where escape depth (e ...) name->t))
             (clause (exists-clause escape depth x param name->t)))
         (if (string? s)
             (string-append s " AND " clause)
             clause)))
      ((_ escape depth ((record-name (reference)) e ...) name->t)
       (let ((s (where escape depth (e ...) name->t))
             (clause (format "~a.~a_id = ~a.id"
                             (name->t 'record-name)
                             (record-name->table-name 'reference)
                             (name->t 'reference))))
         (if (string? s)
             (string-append s " AND " clause)
             clause)))
      ((_ escape depth ((record-name (field-name value) ...) e ...) name->t)
       (fold-left
        (lambda (s clause)
          (if (string? s)
              (string-append s " AND " clause)
              clause))
        (where escape depth (e ...) name->t)
        (list (equality escape
                        (name->t 'record-name)
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
      ((_ escape (record-name (reference foreign) ...) param rest cont)
       (let* ((name->t (->t 0 record-name reference ...))
              (condition (where escape 0 param name->t))
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
                             (join name->t record-name (reference foreign) ...)
                             (if (string? condition)
                                 (string-append " WHERE " condition)
                                 "")
                             tail)))
         (cont c-tuple query)))))

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

  (define (update-query escape rtd names table record)
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

  (define (insert-query escape rtd names table record)
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

  (define (delete-query/id table id)
    (format "DELETE FROM ~a WHERE id = '~d'" table id))

  (define (delete-query table record)
    (delete-query/id table (id-of record)))

)
