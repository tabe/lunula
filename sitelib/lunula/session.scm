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
          account-nick
          account-name
          account-password
          account-mail-address
          account-hash-algorithm
          account-hash-key
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
    (fields nick name password mail-address hash-algorithm hash-key)
    (protocol
     (persistent-protocol
      (lambda (p)
       (lambda (nick name password mail-address hash-algorithm hash-key)
         (p nick
            name
            password
            mail-address
            hash-algorithm
            hash-key))))))

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
                        (account-hash-algorithm a)
                        (account-hash-key a)
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
