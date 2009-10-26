(library (lunula persistent-record)
  (export maybe-id
          maybe-integer
          persistent-record
          persistent-record?
          make-persistent-record
          id-of
          id-set!
          created-at-of
          created-at-set!
          updated-at-of
          updated-at-set!
          define-persistent-record-type
          persistent-protocol)
  (import (rnrs))

  (define (string->id str)
    (cond ((string->number str) => (lambda (id) (and (fixnum? id) (positive? id) id)))
          (else #f)))

  (define (maybe-id x)
    (if (string? x) (string->id x) x))

  (define (maybe-integer x)
    (cond ((integer? x) x)
          ((string? x)
           (let ((n (string->number x)))
             (and (integer? n) n)))
          (else #f)))

  (define-record-type persistent-record
    (fields (mutable id id-of id-set!)
            (mutable created-at created-at-of created-at-set!)
            (mutable updated-at updated-at-of updated-at-set!))
    (protocol
     (lambda (p)
       (lambda (id created-at updated-at)
         (p (maybe-id id)
            created-at
            updated-at)))))

  (define-syntax define-persistent-record-type
    (syntax-rules ()
      ((_ name x ...)
       (define-record-type name
         (parent persistent-record)
         x ...))))

  (define-syntax persistent-protocol
    (syntax-rules ()
      ((_ proc)
       (lambda (n)
         (proc (n #f #f #f))))))

)
