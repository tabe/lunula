(library (lunula uri)
  (export missing-url?
          missing-url-header-of
          url->path
          path-of
          url->fragment
          fragment-of
          url->parameter
          parameter-of
          char-set:reserved
          char-set:unreserved
          decode
          decode-string
          encode
          encode-string)
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

  (define char-set:reserved
    (char-set #\: #\/ #\? #\# #\[ #\] #\@ ; gen-delims
              #\! #\$ #\& #\' #\( #\)     ; sub-delims
              #\* #\+ #\, #\; #\=))

  (define char-set:unreserved
    (char-set-union
     char-set:letter+digit
     (char-set #\- #\. #\_ #\~)))

  (define-condition-type &invalid-percent-encoding &condition
    make-invalid-percent-encoding invalid-percent-encoding?
    (position invalid-percent-encoding-position)
    (sequence invalid-percent-encoding-sequence))

  (define (decode iport oport)
    (assert (textual-port? iport))
    (assert (binary-port? oport))
    (let lp ((n 0)
             (c (get-char iport)))
      (cond ((eof-object? c)
             n)
            ((char=? c #\%)
             (let ((x (get-char iport))
                   (y (get-char iport)))
               (cond ((eof-object? x)
                      (make-invalid-percent-encoding n c))
                     ((eof-object? y)
                      (raise (make-invalid-percent-encoding n (list c x))))
                     ((and (char-set-contains? char-set:hex-digit x)
                           (char-set-contains? char-set:hex-digit y))
                      (let ((i (string->number (format "#x~c~c" x y))))
                        (put-u8 oport i)
                        (lp (+ n 1) (get-char iport))))
                     (else
                      (raise (make-invalid-percent-encoding n (list c x y)))))))
            (else
             (put-u8 oport (char->integer c))
             (lp (+ n 1) (get-char iport))))))

  (define (decode-string str)
    (assert (string? str))
    (call-with-port (open-string-input-port str)
      (lambda (iport)
        (utf8->string
         (call-with-bytevector-output-port
          (lambda (oport)
            (decode iport oport)))))))

  (define (encode iport oport)
    (assert (binary-port? iport))
    (assert (textual-port? oport))
    (let lp ((n 0)
             (b (get-u8 iport)))
      (if (eof-object? b)
          n
          (cond ((and (< b #x80)
                      (let ((c (integer->char b)))
                        (and (char-set-contains? char-set:unreserved c)
                             c)))
                 (lambda (c)
                   (put-char oport c)
                   (lp (+ n 1) (get-u8 iport))))
                (else
                 (format oport (if (< b 16) "%0~x" "%~x") b)
                 (lp (+ n 3) (get-u8 iport)))))))

  (define (encode-string str)
    (assert (string? str))
    (call-with-port (open-bytevector-input-port (string->utf8 str))
      (lambda (iport)
        (call-with-string-output-port
         (lambda (oport)
           (encode iport oport))))))

)
