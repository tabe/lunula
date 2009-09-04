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
          account-name
          account-nick
          account-password
          account-mail-address
          account-algorithm
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
          (lunula concurrent)
          (lunula persistent-record))

  (define-record-type confirmation
    (fields (immutable ok ok?)))

  (define-persistent-record-type account
    (fields nick name password mail-address algorithm)
    (protocol
     (persistent-protocol
      (lambda (p)
       (lambda (nick name password mail-address algorithm)
         (p nick
            name
            password
            mail-address
            "clear"))))))

  (define-record-type user
    (fields account))

  (define-record-type session
    (fields user uuid))

  (define *logged-in* (make-messenger-bag 1024))

  (define (generate-session a)
    (assert (account? a))
    (let ((uuid (make-uuid))
          (params (list (id-of a)
                        (account-nick a)
                        (account-name a)
                        (account-password a)
                        (account-mail-address a)
                        (account-algorithm a)
                        )))
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
                (let ((a (apply make-account (cdr params))))
                  (id-set! a (car params))
                  (generate-session a))))))

)
