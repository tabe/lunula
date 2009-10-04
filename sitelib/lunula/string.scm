(library (lunula string)
  (export blank?
          string-underscore
          string-hyphen
          string-truncate)
  (import (rnrs)
          (only (srfi :13) string-map string-null?))

  (define (blank? x)
    (or (not x)
        (and (string? x) (string-null? x))))

  (define (string-underscore str)
    (string-map (lambda (c) (if (char=? #\- c) #\_ c)) str))

  (define (string-hyphen str)
    (string-map (lambda (c) (if (char=? #\_ c) #\- c)) str))

  (define (string-truncate str max)
    (let ((len (string-length str)))
      (if (<= len max)
          str
          (string-append (substring str 0 max) "..."))))

)
