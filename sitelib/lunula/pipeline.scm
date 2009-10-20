(library (lunula pipeline)
  (export request-drop!
          request-get!
          request-put!
          response-get!
          response-put!)
  (import (rnrs)
          (only (ypsilon concurrent) make-messenger-bag messenger-bag-get! messenger-bag-put!)
          (only (lunula concurrent) messenger-bag-get-gracefully!))

  (define *request* (make-messenger-bag 10))

  (define (request-put! path request)
    (messenger-bag-put! *request* path request))

  (define (request-get! path timeout)
    (messenger-bag-get! *request* path timeout))

  (define (request-drop! path)
    (messenger-bag-get-gracefully! *request* path 100))

  (define *response* (make-messenger-bag 10))

  (define (response-get! key)
    (messenger-bag-get! *response* key))

  (define (response-put! key response)
    (messenger-bag-put! *response* key response))

)
