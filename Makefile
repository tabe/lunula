PROG = ypsilon

PREFIX = /usr/local

YPSILON = $(PROG) --sitelib=sitelib --heap-limit=16

.PHONY: check install uninstall test stats

check: test

install:
	mkdir -p -m755 $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib
	find sitelib -type f -name '*.scm' | cpio -pdu $(DESTDIR)$(PREFIX)/share/$(PROG)
	find $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib -type d -exec chmod 755 {} \;
	find $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib -type f -exec chmod 644 {} \;

uninstall:
	-rm -rf $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib/lunula.scm
	-rm -rf $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib/lunula

test:
	$(YPSILON) tests/lunula/concurrent.scm
	$(YPSILON) tests/lunula/gettext.scm
	$(YPSILON) tests/lunula/html.scm
	$(YPSILON) tests/lunula/log.scm
	$(YPSILON) tests/lunula/mod_lisp.scm
	$(YPSILON) tests/lunula/mysql.scm
	$(YPSILON) tests/lunula/session.scm
	$(YPSILON) tests/lunula/string.scm
	$(YPSILON) tests/lunula/tree.scm
	$(YPSILON) tests/lunula/uri.scm
	$(YPSILON) tests/lunula.scm

stats:
	wc -l sitelib/lunula/*.scm sitelib/lunula.scm
