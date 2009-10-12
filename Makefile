PROG = ypsilon

PREFIX = /usr/local

SITELIB = sitelib:submodules/base64:submodules/uri:submodules/xunit:submodules/ypsilon-foreign-lib/sitelib

YPSILON = env LUNULA_CONFIGURATION_DIRECTORY=config $(PROG) --sitelib=$(SITELIB) --heap-limit=16

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
	$(YPSILON) tests/lunula/configuration.scm
	$(YPSILON) tests/lunula/gettext.scm
	$(YPSILON) tests/lunula/hmac.scm
	$(YPSILON) tests/lunula/html.scm
	$(YPSILON) tests/lunula/log.scm
	$(YPSILON) tests/lunula/mod_lisp.scm
	$(YPSILON) tests/lunula/mysql.scm
	$(YPSILON) tests/lunula/persistent-record.scm
	$(YPSILON) tests/lunula/rss.scm
	$(YPSILON) tests/lunula/sendmail.scm
	$(YPSILON) tests/lunula/session.scm
	$(YPSILON) tests/lunula/string.scm
	$(YPSILON) tests/lunula/template.scm
	$(YPSILON) tests/lunula/tree.scm
	$(YPSILON) tests/lunula/uri.scm
	$(YPSILON) tests/lunula/validation.scm
	$(YPSILON) tests/lunula/xml.scm
	$(YPSILON) tests/lunula.scm

stats:
	wc -l sitelib/lunula/*.scm sitelib/lunula.scm
	wc -l tests/lunula/*.scm tests/lunula.scm
