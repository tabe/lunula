(library (lunula log)
  (export drain
          info)
  (import (rnrs)
          (only (core) make-parameter format))

  (define drain (make-parameter #t))

  (define (info msg . args)
    (assert (string? msg))
    (apply format (drain) (string-append msg "~&") args))

)
