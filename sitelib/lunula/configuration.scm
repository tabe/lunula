(library (lunula configuration)
  (export define-configuration)
  (import (rnrs)
          (only (srfi :13) string-suffix?)
          (only (core) directory-list lookup-process-environment))

  (define *configuration-directory*
    (lookup-process-environment "LUNULA_CONFIGURATION_DIRECTORY"))

  (define *configurations*
    (let ((ht (make-eq-hashtable 10)))
      (when *configuration-directory*
        (for-each
         (lambda (name)
           (when (string-suffix? ".scm" name)
             (call-with-input-file (string-append *configuration-directory* "/" name)
               (lambda (port)
                 (let loop ((x (read port)))
                   (cond ((eof-object? x)
                          #f)
                         ((and (list? x)
                               (<= 2 (length x)))
                          (hashtable-set! ht (car x) (cadr x))
                          (loop (read port)))))))))
         (directory-list *configuration-directory*)))
      ht))

  (define get-configuration
    (case-lambda
     ((name)
      (hashtable-ref *configurations* name #f))
     ((name default)
      (hashtable-ref *configurations* name default))))

  (define-syntax define-configuration
    (syntax-rules ()
      ((_ name)
       (define name (get-configuration 'name)))
      ((_ name default)
       (define name (get-configuration 'name default)))))

)
