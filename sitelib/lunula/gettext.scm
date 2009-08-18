(library (lunula gettext)
  (export __
          gettext
          locale)
  (import (core) (rnrs))

  (define *default-locale* 'en)

  (define *locale-table*
    (let ((table (make-eq-hashtable 2)))
      (hashtable-set! table 'en (make-eq-hashtable))
      (hashtable-set! table 'ja (make-eq-hashtable))
      table))

  (define locale (make-parameter *default-locale*))

  (define-syntax __
    (syntax-rules ()
      ((_ msgid)
       (let ((table (hashtable-ref *locale-table* (locale) *default-locale*)))
         (hashtable-ref table 'msgid (symbol->string 'msgid))))))

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
