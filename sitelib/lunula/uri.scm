(library (lunula uri)
  (export url->fragment
          url->parameter
          url->path)
  (import (rnrs)
          (match)
          (only (pregexp) pregexp-match)
          (only (srfi :13) string-index)
          (only (srfi :14) char-set))

  (define (url->fragment url)
    (match (pregexp-match "#([[:alnum:]_-]+)" url)
      ((_ fragment) fragment)
      (else #f)))

  (define (url->parameter url)
    (match (pregexp-match "?([[:alnum:]_-]+)" url)
      ((_ parameter) parameter)
      (else #f)))

  (define (url->path url)
    (let ((i (string-index url (char-set #\? #\#))))
      (if i (substring url 0 i) url)))

)
