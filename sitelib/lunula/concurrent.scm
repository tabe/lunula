(library (lunula concurrent)
  (export messenger-bag-get-gracefully!)
  (import (rnrs)
          (only (concurrent) messenger-bag-get!))

  (define (messenger-bag-get-gracefully! bag tag timeout . default)
    (guard (con
            ((and (condition? con)
                  (eq? 'messenger-bag-get! (condition-who con)))
             (if (null? default)
                 con
                 (apply values default))))
      (messenger-bag-get! bag tag timeout)))

  )
