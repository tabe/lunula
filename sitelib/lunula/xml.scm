(library (lunula xml)
  (export declaration
          default-attributes
          define-element
          define-element/
          escape
          escape-char
          escape-string)
  (import (rnrs)
          (only (srfi :1) lset-union))

  (define-syntax declaration
    (syntax-rules ()
      ((_ version encoding)
       `("<?xml version='" ,version "' encoding='" ,encoding "'?>\n"))))

  (define default-attributes (make-eq-hashtable))

  (define (alist-merge orig new)
    (map
     (lambda (k) (or (assq k new) (assq k orig)))
     (lset-union eq? (map car orig) (map car new))))

  (define (pair->attr kv)
    (cond ((cdr kv)
           => (lambda (v)
                (if (boolean? v)
                    `(#\space ,(car kv))
                    `(#\space ,(car kv) "='" ,v "'"))))
          (else '())))

  (define (alist->attributes alist name)
    (map
     pair->attr
     (alist-merge (hashtable-ref default-attributes name '()) alist)))

  (define (element/attributes name alist)
    `(#\< ,name ,@(alist->attributes alist name) " />"))

  (define (element/attributes/ name alist)
    `(#\< ,name ,@(alist->attributes alist name) "></" ,name "\n>"))

  (define (element/attributes/nodes name alist . nodes)
    `(#\< ,name ,@(alist->attributes alist name) #\> ,@nodes "</" ,name "\n>"))

  (define-syntax define-element
    (syntax-rules ()
      ((_ name ...)
       (begin
         (define-syntax name
           (syntax-rules ()
             ((_)
              (element/attributes 'name '()))
             ((_ ((k v) (... ...)))
              (element/attributes 'name `((k . ,v) (... ...))))
             ((_ ((k v) (... ...)) e0 (... ...))
              (element/attributes/nodes 'name `((k . ,v) (... ...)) e0 (... ...)))
             ((_ e0 (... ...))
              (element/attributes/nodes 'name '() e0 (... ...)))))
         ...))))

  (define-syntax define-element/
    (syntax-rules ()
      ((_ name ...)
       (begin
         (define-syntax name
           (syntax-rules ()
             ((_)
              (element/attributes/nodes 'name '()))
             ((_ ((k v) (... ...)))
              (element/attributes/ 'name `((k . ,v) (... ...))))
             ((_ ((k v) (... ...)) e0 (... ...))
              (element/attributes/nodes 'name `((k . ,v) (... ...)) e0 (... ...)))
             ((_ e0 (... ...))
              (element/attributes/nodes 'name '() e0 (... ...)))))
         ...))))


  (define *special-chars* (make-eqv-hashtable))

  (define (escape iport oport)
    (assert (textual-port? iport))
    (assert (textual-port? oport))
    (let loop ((c (get-char iport)))
      (cond ((eof-object? c)
             #t)
            ((hashtable-ref *special-chars* c #f)
             => (lambda (e)
                  (put-string oport e)
                  (loop (get-char iport))))
            (else
             (put-char oport c)
             (loop (get-char iport))))))

  (define (escape-char c)
    (hashtable-ref *special-chars* c c))

  (define (escape-string str)
    (call-with-port (open-string-input-port str)
      (lambda (iport)
        (call-with-string-output-port
         (lambda (oport)
           (escape iport oport))))))

  (for-each
   (lambda (k v)
     (hashtable-set! *special-chars* k v))
   '(#\& #\" #\' #\< #\>)
   '("&amp;" "&quot;" "&#039;" "&lt;" "&gt;"))

)
