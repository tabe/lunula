(library (lunula mod_lisp)
  (export get-header
          premature-end-of-header?
          put-header)
  (import (rnrs)
          (only (srfi :1) drop take unfold))

  (define-condition-type &premature-end-of-header &condition
    make-premature-end-of-header premature-end-of-header?)

  (define (binary-get-line port)
    (let lp ((x '()))
      (let ((b (get-u8 port)))
        (cond ((eof-object? b)
               (raise (make-premature-end-of-header)))
              ((= 10 b)
               (utf8->string (u8-list->bytevector (reverse x))))
              (else
               (lp (cons b x)))))))

  (define (get-header port)
    (assert (binary-port? port))
    (let ((ls (unfold (lambda (x) (cond ((eof-object? x)
                                         (close-port port)
                                         (raise (make-premature-end-of-header)))
                                        (else
                                         (string=? "end" x))))
                      (lambda (x) x)
                      (lambda (x) (binary-get-line port))
                      (binary-get-line port))))
      (if (even? (length ls))
          (unfold null?
                  (lambda (x) (take x 2))
                  (lambda (x) (drop x 2))
                  ls)
          (raise (make-premature-end-of-header)))))

  (define (put-header port header)
    (assert (textual-port? port))
    (for-each
     (lambda (pair)
       (display (car pair) port)
       (newline port)
       (display (cdr pair) port)
       (newline port))
     header)
    (put-string port "end\n"))

)
