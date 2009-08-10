(library (lunula html)
  (export doctype
          blockquote
          body
          dd
          div
          dl
          dt
          em
          head
          html
          li
          ol
          p
          q
          span
          title
          ul)
  (import (except (rnrs) div))

  (define (doctype type)
    (case type
      ((strict)
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n\t\"http://www.w3.org/TR/html4/strict.dtd\">\n")
      ((transitional)
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n\t\"http://www.w3.org/TR/html4/loose.dtd\">\n")
      ((xhtml-1.0-strict)
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
      ((xhtml-1.0-transitional)
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
      ((xhtml-1.1)
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"\n\t\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n")))

  (define-syntax define-element
    (syntax-rules ()
      ((_ name ...)
       (begin
         (define-syntax name
           (syntax-rules ()
             ((_ ((k v) (... ...)) e0 (... ...))
              `(#\< name
                (#\space k "='" ,v "'") (... ...)
                #\>
                ,e0 (... ...)
                "</" name "\n>"))
             ((_ e0 (... ...))
              `(#\< name #\>
                ,e0 (... ...)
                "</" name "\n>"))))
         ...))))

  (define-element
    blockquote
    body
    dd
    div
    dl
    dt
    em
    head
    html
    li
    ol
    p
    q
    span
    title
    ul)

)
