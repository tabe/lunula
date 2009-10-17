(library (lunula tree)
  (export put-tree
          tree->string)
  (import (rnrs))

  (define (put-tree oport tree . args)
    (assert (textual-port? oport))
    (cond ((null? tree)
           #t)
          ((pair? tree)
           (begin
             (apply put-tree oport (car tree) args)
             (apply put-tree oport (cdr tree) args)))
          ((symbol? tree)
           (put-string oport (symbol->string tree)))
          ((char? tree)
           (put-char oport tree))
          ((string? tree)
           (put-string oport tree))
          ((number? tree)
           (display tree oport))
          ((vector? tree)
           (vector-for-each
            (lambda (x) (apply put-tree oport x args))
            tree))
          ((procedure? tree)
           (apply put-tree oport (apply tree args) args))
          (else
           (error 'put-tree "invalid tree" tree))))

  (define (tree->string tree . args)
    (call-with-string-output-port
     (lambda (oport)
       (apply put-tree oport tree args))))

)
