(library (lunula gettext)
  (export __
          ___
          gettext
          locale
          localize)
  (import (only (core) make-parameter parameterize)
          (rnrs))

  (define *default-locale* 'en)

  (define *locale-table*
    (let ((table (make-eq-hashtable 2)))
      (hashtable-set! table 'en (make-eq-hashtable))
      (hashtable-set! table 'ja (make-eq-hashtable))
      table))

  (define *locale* (make-parameter *default-locale*))

  (define-syntax locale
    (syntax-rules ()
      ((_ loc) (*locale* 'loc))
      ((_) (*locale*))))

  (define (___ msgid)
    (cond ((hashtable-ref *locale-table* (locale) #f)
           => (lambda (table)
                (or (hashtable-ref table msgid #f)
                    (symbol->string msgid))))
          (else (symbol->string msgid))))

  (define-syntax __
    (syntax-rules ()
      ((_ msgid)
       (___ 'msgid))))

  (define-syntax localize
    (syntax-rules ()
      ((_ lang e0 e1 ...)
       (let ((x lang))
         (cond ((symbol? x)
                (parameterize ((*locale* x))
                  e0 e1 ...))
               (else e0 e1 ...))))))

  (define-syntax gettext
    (syntax-rules ()
      ((_ (msgid (loc msgstr) ...) clause ...)
       (begin
         (let ((table (hashtable-ref *locale-table* 'loc #f)))
           (if (hashtable? table)
               (hashtable-set! table 'msgid msgstr)))
         ...
         (gettext clause ...)))
      ((_)
       #f)))

)
