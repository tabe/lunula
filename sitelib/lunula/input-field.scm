(library (lunula input-field)
  (export add-input-fields
          input-types
          input-descriptions
          input-field)
  (import (rnrs (6))
          (only (srfi :28) format)
          (match)
          (only (ypsilon concurrent) make-uuid)
          (prefix (lunula html) html:))

  (define *input-types* (make-eq-hashtable 10))

  (define *input-descriptions* (make-eq-hashtable 10))

  (define-syntax split-input-specification
    (syntax-rules ()
      ((_ (t d) () proc)
       (proc (reverse t) (reverse d)))
      ((_ (t d) (#f e ...) proc)
       (split-input-specification ((cons #f t) (cons #f d)) (e ...) proc))
      ((_ (t d) ((type) e ...) proc)
       (split-input-specification ((cons 'type t) (cons #f d)) (e ...) proc))
      ((_ (t d) ((type desc) e ...) proc)
       (split-input-specification ((cons 'type t) (cons desc d)) (e ...) proc))
      ((_ (t d) (x e ...) proc)
       (split-input-specification ((cons x t) (cons #f d)) (e ...) proc))))

  (define-syntax add-input-fields
    (syntax-rules ()
      ((_ record-name (e ...))
       (split-input-specification
        ('() '())
        (e ...)
        (lambda (t d)
          (let ((rtd (record-type-descriptor record-name)))
            (hashtable-set! *input-types* rtd t)
            (hashtable-set! *input-descriptions* rtd d)))))))

  (define (input-types rtd)
    (hashtable-ref *input-types* rtd '()))

  (define (input-descriptions rtd)
    (hashtable-ref *input-descriptions* rtd '()))

  (define (radio-buttons name v ls)

    (define (radio-button x y)
      (let ((id (make-uuid)))
        (list (html:input ((type 'radio) (name name) (value (car x)) (id id) (checked y)))
              (html:label ((for id)) (cadr x))
              "&nbsp;")))

    (let ((checked (map (lambda (x) (string=? (format "~a" v) (format "~a" (car x)))) ls)))
      (if (exists values checked)
          (map radio-button ls checked)
          (map (lambda (x) (radio-button x (caddr x))) ls))))

  (define (input-field type name v)
    (cond ((list? type)
           (match type
             (('radio . ls)
              (radio-buttons name v ls))
             (('select . ls)
              (html:select
               ((name name))
               (map
                (lambda (x)
                  (let ((selected (string=? (format "~a" v) (format "~a" (car x)))))
                    (html:option ((value (car x)) (selected selected)) (cadr x))))
                ls)))))
          ((eq? 'textarea type)
           (html:textarea ((name name)) v))
          ((symbol? type)
           (case type
             ((password) (html:input ((type type) (name name))))
             (else (html:input ((type type) (name name) (value v))))))
          (else (error 'input-field "invalid type" (list name type v)))))

)
