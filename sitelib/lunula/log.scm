(library (lunula log)
  (export drain
          info)
  (import (rnrs)
          (only (core) make-parameter format lookup-process-environment))

  (define drain
    (make-parameter
     (cond ((lookup-process-environment "LUNULA_LOG_DRAIN")
            => (lambda (path) (open-file-output-port path (file-options no-fail))))
           (else #t))))

  (define (info msg . args)
    (assert (string? msg))
    (let ((port (drain)))
      (apply format port (string-append msg "~%") args)
      (when (output-port? port) (flush-output-port port))))

)
