(library (lunula tree)
  (export put-tree
          tree->string)
  (import (rnrs))

  (define (put-tree oport tree)
    (assert (textual-port? oport))
    (for-each
     (lambda (node)
       (cond ((list? node)
              (put-tree oport node))
             ((symbol? node)
              (put-string oport (symbol->string node)))
             ((string? node)
              (put-string oport node))
             ((char? node)
              (put-char oport node))
             (else
              (display node oport))))
     tree))

  (define (tree->string tree)
    (call-with-string-output-port
     (lambda (oport)
       (put-tree oport tree))))

)
