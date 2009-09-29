#!/usr/bin/env ypsilon
#!r6rs

(import (lunula tree)
        (lunula xml)
        (xunit))

(assert-string=? "<?xml version='1.0' encoding='UTF-8'?>" (tree->string (declaration 1.0 "UTF-8")))

(report)
