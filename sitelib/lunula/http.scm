(library (lunula http)
  (export string->accept-language
          accept-language->string)
  (import (rnrs (6))
          (only (srfi :13)
                string-join
                string-prefix?
                string-tokenize)
          (only (srfi :14)
                char-set
                char-set-complement
                char-set-union
                char-set:whitespace))

  (define string->accept-language
    (let ((outer (char-set-complement (char-set-union (char-set #\,) char-set:whitespace)))
          (inner (char-set-complement (char-set #\;))))
      (lambda (s)
        (map
         (lambda (x)
           (let* ((ls (string-tokenize x inner))
                  (len (length ls)))
             (cond ((= 1 len) ls)
                   ((and (= 2 len)
                         (string-prefix? "q=" (cadr ls)))
                    (let ((q (cadr ls)))
                      (list (car ls) (substring q 2 (string-length q)))))
                   (else
                    (raise (condition
                            (make-warning)
                            (make-irritants-condition x)
                            (make-message-condition "invalid Accept-Language's value")))))))
         (string-tokenize s outer)))))

  (define (accept-language->string ls)
    (string-join
     (map
      (lambda (x)
        (case (length x)
          ((1) (car x))
          ((2) (string-append (car x) ";q=" (cadr x)))
          (else
           (raise (condition
                   (make-warning)
                   (make-irritants-condition ls)
                   (make-message-condition "invalid Accept-Language's value"))))))
      ls)
     ", "))

)
