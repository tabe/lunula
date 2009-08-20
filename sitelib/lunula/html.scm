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
          ul)
  (import (except (rnrs) div))

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

  (define-syntax define-element
    (syntax-rules ()
      ((_ name ...)
       (begin
         (define-syntax name
           (syntax-rules ()
             ((_)
              `(#\< name " />"))
             ((_ ((k v) (... ...)))
              `(#\< name
                (#\space k "='" ,v "'") (... ...)
                " />"))
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

  (define-syntax define-element/
    (syntax-rules ()
      ((_ name ...)
       (begin
         (define-syntax name
           (syntax-rules ()
             ((_)
              `(#\< name "></" name "\n>"))
             ((_ ((k v) (... ...)))
              `(#\< name
                (#\space k "='" ,v "'") (... ...)
                "></" name "\n>"))
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
    script
    textarea
    table
    tbody
    tfoot)

)
