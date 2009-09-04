#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (only (lunula persistent-record) id-set!)
        (lunula session)
        (xunit))

(define a (make-account "foo" "bazbazbaz" "password" "mail-address" "algorithm"))
(id-set! a 123)

(assert (not (logged-in? #f)))
(assert (not (logged-in? "bad")))
(let ((sess (do-login a)))
  (let ((sess (logged-in? (session-uuid sess))))
    (assert (session? sess))
    (let ((params (do-logout sess)))
      (assert-equal? '(123 "foo" "bazbazbaz" "password" "mail-address" "clear") params)
      (assert (not (logged-in? (session-uuid sess)))))))

(report)
