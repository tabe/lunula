#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (only (core) format)
        (lunula hmac)
        (xunit))

(define *key* "1234567890")
(define *string*
  (format (string-append
           "GET~%webservices.amazon.com~%/onca/xml~%AWSAccessKeyId=~a&"
           "ItemId=~a&"
           "Operation=~a&"
           "ResponseGroup=~a&"
           "Service=~a&"
           "Timestamp=~a&"
           "Version=~a")
          "00000000000000000000"
          "0679722769"
          "ItemLookup"
          "ItemAttributes%2COffers%2CImages%2CReviews"
          "AWSECommerceService"
          "2009-01-01T12%3A00%3A00Z"
          "2009-01-06"))
(define *data* (string->utf8 *string*))
(define *expected* "Nace+U3Az4OhN7tISqgs1vdLBHBEijWcBeCqL5xN9xg=")
(assert-string=? *expected* (sha-256 *key* *data*))

(report)
