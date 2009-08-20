(library (lunula string)
  (export string-underscore string-hyphen)
  (import (rnrs)
          (only (srfi :13) string-map))

  (define (string-underscore str)
    (string-map (lambda (c) (if (char=? #\- c) #\_ c)) str))

  (define (string-hyphen str)
    (string-map (lambda (c) (if (char=? #\_ c) #\- c)) str))
    
)
