(library (lunula template)
  (export templates
          template-environment
          load-templates
          template->tree)
  (import (rnrs)
          (rnrs eval)
          (only (core) directory-list format lookup-process-environment make-parameter)
          (only (srfi :1) unfold)
          (only (srfi :13) string-suffix?))

  (define templates (make-parameter (lookup-process-environment "LUNULA_TEMPLATES")))

  (define *template-environment*
    (make-parameter
     '((except (rnrs) div)
       (lunula html))))

  (define-syntax template-environment
    (syntax-rules ()
      ((_ e0 e1 ...)
       (*template-environment* '(e0 e1 ...)))))

  (define *template-cache* (make-eqv-hashtable 10))

  (define (path->template path)
    (call-with-input-file path
      (lambda (port)
        (unfold eof-object?
                values
                (lambda _ (read port))
                (read port)))))

  (define (load-templates force)
    (let ((touched (format "~a/00TOUCHED" (templates))))
      (when (or force (not (file-exists? touched)))
        (for-each
         (lambda (f)
           (when (string-suffix? ".scm" f)
             (let* ((name (substring f 0 (- (string-length f) 4)))
                    (template (or (string->number name) (string->symbol name)))
                    (path (format "~a/~a" (templates) f))
                    (forest (path->template path))
                    (evaluated (eval (cons 'list forest) (apply environment (*template-environment*)))))
               (hashtable-set! *template-cache* template evaluated))))
         (directory-list (templates)))
        (call-with-port (open-file-output-port touched (file-options no-fail))
          values))))

  (define (template->tree name)
    (hashtable-ref *template-cache* name '()))

)
