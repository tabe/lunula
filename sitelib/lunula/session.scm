(library (lunula session)
  (export session
          session?
          make-session
          session-user
          session-uuid
          confirmation
          ok?
          account
          account?
          make-account
          account-id
          account-name
          account-password
          user
          user?
          user-account
          make-user
          do-login
          do-logout
          logged-in?)
  (import (rnrs)
          (core)
          (concurrent)
          (lunula concurrent))

  (define-record-type confirmation
    (fields (immutable ok ok?)))

  (define-record-type account
    (fields id name password)
    (protocol
     (lambda (p)
       (lambda (x y z)
         (p (if (string? x) (string->number x) x)
            y
            z)))))

  (define-record-type user
    (fields account))

  (define-record-type session
    (fields user uuid))

  (define *logged-in* (make-messenger-bag 1024))

  (define (generate-session a)
    (assert (account? a))
    (let ((uuid (make-uuid))
          (params (list (account-id a)
                        (account-name a)
                        (account-password a))))
      (messenger-bag-put! *logged-in* uuid params 100)
      (make-session (make-user a) uuid)))

  (define (do-login a)
    (assert (account? a))
    (generate-session a))

  (define (do-logout sess)
    (assert (session? sess))
    (messenger-bag-get-gracefully! *logged-in* (session-uuid sess) 100))

  (define (logged-in? x)
    (and (string? x)
         (let ((params (messenger-bag-get-gracefully! *logged-in* x 100 #f)))
           (and (list? params)
                (generate-session (apply make-account params))))))

)
