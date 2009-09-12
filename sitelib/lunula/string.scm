(library (lunula string)
  (export blank? string-underscore string-hyphen)
  (import (rnrs)
          (only (srfi :13) string-map string-null?))

  (define (blank? x)
    (or (not x)
        (and (string? x) (string-null? x))))

  (define (string-underscore str)
    (string-map (lambda (c) (if (char=? #\- c) #\_ c)) str))

  (define (string-hyphen str)
    (string-map (lambda (c) (if (char=? #\_ c) #\- c)) str))
    
)
