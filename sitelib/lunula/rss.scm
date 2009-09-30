(library (lunula rss)
  (export channel
          dc:date
          dc:subject
          description
          guid
          item
          items
          language
          link
          rdf:li
          rdf:RDF
          rdf:Seq
          title)
  (import (rnrs)
          (lunula xml))

  (define-element
    channel
    dc:date
    dc:subject
    description
    guid
    item
    items
    language
    link
    rdf:li
    rdf:RDF
    rdf:Seq
    title)

  (for-each
   (lambda (k v)
     (hashtable-set! default-attributes k v))
   '(rdf:RDF)
   '(((xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      (xmlns:dc "http://purl.org/dc/elements/1.1/")
      (xmlns "http://purl.org/rss/1.0/"))))

)
