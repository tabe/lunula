(library (lunula html)
  (export doctype
          a
          blockquote
          body
          br
          code
          dd
          div
          dl
          dt
          em
          form
          h1
          h2
          h3
          h4
          h5
          h6
          head
          hr
          html
          image
          input
          label
          li
          link
          meta
          ol
          p
          pre
          q
          script
          span
          style
          table
          tbody
          td
          textarea
          tfoot
          th
          title
          tr
          ul
          escape
          escape-char
          escape-string)
  (import (except (rnrs) div)
          (only (srfi :1) lset-union))

  (define-syntax doctype
    (syntax-rules (strict transitional xhtml-1.0-strict xhtml-1.0-transitional xhtml-1.1)
      ((_ strict)
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n\t\"http://www.w3.org/TR/html4/strict.dtd\">\n")
      ((_ transitional)
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n\t\"http://www.w3.org/TR/html4/loose.dtd\">\n")
      ((_ xhtml-1.0-strict)
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
      ((_ xhtml-1.0-transitional)
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
      ((_ xhtml-1.1)
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"\n\t\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n")))

  (define *default-attributes* (make-eq-hashtable))

  (define (alist-merge orig new)
    (map
     (lambda (k) (or (assq k new) (assq k orig)))
     (lset-union eq? (map car orig) (map car new))))

  (define (pair->attr kv)
    `(#\space ,(car kv) "='" ,(cdr kv) "'"))

  (define (alist->attributes alist name)
    (map
     pair->attr
     (alist-merge (hashtable-ref *default-attributes* name '()) alist)))

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

  (define-element
    a
    blockquote
    body
    br
    code
    dd
    div
    dl
    dt
    em
    h1
    h2
    h3
    h4
    h5
    h6
    head
    hr
    html
    image
    input
    li
    link
    meta
    ol
    p
    pre
    q
    span
    style
    title
    td
    th
    tr
    ul)

  (define-element/
    form
    label
    script
    textarea
    table
    tbody
    tfoot)

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
     (hashtable-set! *default-attributes* k v))
   '(form
     textarea)
   '(((method . "POST"))
     ((rows . 5) (cols . 50))))

  (for-each
   (lambda (k v)
     (hashtable-set! *special-chars* k v))
   '(#\& #\" #\' #\< #\>)
   '("&amp;" "&quot;" "&#039;" "&lt;" "&gt;"))

)
