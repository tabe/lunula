(library (lunula uri)
  (export missing-url?
          missing-url-header-of
          url->path
          path-of
          url->fragment
          fragment-of
          url->parameter
          parameter-of)
  (import (only (core) format)
          (rnrs)
          (match)
          (only (pregexp) pregexp-match)
          (only (srfi :13) string-index)
          (srfi :14))

  (define-condition-type &missing-url &condition
    make-missing-url missing-url?
    (header missing-url-header-of))

  (define (url-of header)
    (let ((u (assoc "url" header)))
      (if u
          (cadr u)
          (raise (make-missing-url header)))))

  (define (url->path url)
    (let ((i (string-index url (char-set #\? #\#))))
      (if i (substring url 0 i) url)))

  (define (path-of header)
    (url->path (url-of header)))

  (define (url->fragment url)
    (match (pregexp-match "#([[:alnum:]_-]+)" url)
      ((_ fragment) fragment)
      (else #f)))

  (define (fragment-of header)
    (url->fragment (url-of header)))

  (define (url->parameter url)
    (match (pregexp-match "?([[:alnum:]_-]+)" url)
      ((_ parameter) parameter)
      (else #f)))

  (define (parameter-of header)
    (url->parameter (url-of header)))

)
