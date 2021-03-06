#!r6rs

(import (rnrs)
        (ypsilon gcrypt)
        (lunula md)
        (xunit))

(gcry_check_version "1.4.5") ; you might modify the version
(gcry_control GCRYCTL_DISABLE_SECMEM)
(gcry_control GCRYCTL_INITIALIZATION_FINISHED)

(define-syntax assert-md5
  (syntax-rules ()
    ((_ expected str)
     (assert-string=? expected (md5 (string->utf8 str))))))

(assert-md5 "a1ea9cf0ad8079d6d9385aa1af02113f" "Lunula")
(assert-md5 "a6ad28d2dc385808b0b8443239899534" "lunula")
(assert-md5 "e9b1713db620f1e3a14b6812de523f4b" "0123456789abcdefghijklmnopqrstuvwxyz")

(report)
