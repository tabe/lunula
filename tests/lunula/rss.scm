#!/usr/bin/env ypsilon
#!r6rs

(import (lunula tree)
        (lunula rss)
        (xunit))

(assert-string=? "<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns='http://purl.org/rss/1.0/'>...</rdf:RDF\n>" (tree->string (rdf:RDF "...")))

(report)
