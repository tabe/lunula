#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs) (lunula session))

(define a (make-account "123" "foo" "bazbazbaz"))

(assert (not (logged-in? #f)))
(assert (not (logged-in? "bad")))
(let ((sess (do-login a)))
  (let ((sess (logged-in? (session-uuid sess))))
    (assert (session? sess))
    (let ((params (do-logout sess)))
      (assert (equal? '(123 "foo" "bazbazbaz") params))
      (assert (not (logged-in? (session-uuid sess)))))))
