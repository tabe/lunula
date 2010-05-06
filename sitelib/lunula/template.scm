(library (lunula template)
  (export templates
          template-locale
          template-environment
          load-templates
          template->tree)
  (import (rnrs)
          (rnrs eval)
          (only (core) directory-list format lookup-process-environment make-parameter)
          (only (srfi :1) unfold)
          (only (srfi :13) string-suffix?))

  (define templates (make-parameter (lookup-process-environment "LUNULA_TEMPLATES")))

  (define *template-locale* (make-parameter '(en ja)))

  (define-syntax template-locale
    (syntax-rules ()
      ((_ lang0 lang1 ...)
       (*template-locale* '(lang0 lang1 ...)))))

  (define *template-environment*
    (make-parameter
     '((except (rnrs) div)
       (lunula gettext)
       (lunula html))))

  (define-syntax template-environment
    (syntax-rules ()
      ((_ e0 e1 ...)
       (*template-environment* '(e0 e1 ...)))))

  (define *template-cache* (make-eq-hashtable 2))

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
         (lambda (lang)
           (let ((ht (make-eqv-hashtable 10)))
             (for-each
              (lambda (f)
                (call/cc
                 (lambda (ret)
                   (for-each
                    (lambda (ext)
                      (when (string-suffix? ext f)
                        (let* ((name (substring f 0 (- (string-length f) (string-length ext))))
                               (template (or (string->number name) (string->symbol name)))
                               (path (format "~a/~a" (templates) f))
                               (forest (path->template path))
                               (evaluated (eval (list 'localize (list 'quote lang) (cons 'list forest)) (apply environment (*template-environment*)))))
                          (hashtable-set! ht template evaluated))
                        (ret ext)))
                    `(,(format ".scm.~a" lang)
                      ".scm")))))
              (directory-list (templates)))
             (hashtable-set! *template-cache* lang ht)))
         (*template-locale*))
        (call-with-port (open-file-output-port touched (file-options no-fail))
          values))))

  (define (template->tree name lang)
    (cond ((hashtable-ref *template-cache* (or lang (car (*template-locale*))) #f)
           => (lambda (ht) (hashtable-ref ht name '())))
          (else '())))

)
