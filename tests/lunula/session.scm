#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (only (lunula persistent-record) id-set!)
        (lunula session)
        (xunit))

(define *password* "IqZb01fzvt/28tLn7xstR2iQ3+eVuL+R/gZm+4P/myk=")
(define *key* "20113fc6-9633-4926-988b-d5e31c32d00c")

(define a (make-account "foo" "bazbazbaz" *password* "mail-address" "sha-256" *key*))
(id-set! a 123)

(assert (not (logged-in? #f)))
(assert (not (logged-in? "bad")))
(let ((sess (do-login a)))
  (let ((sess (logged-in? (session-uuid sess))))
    (assert (session? sess))
    (let ((params (do-logout sess)))
      (assert-equal? (list 123 "foo" "bazbazbaz" *password* "mail-address" "sha-256" *key*) params)
      (assert (not (logged-in? (session-uuid sess)))))))

(report)
