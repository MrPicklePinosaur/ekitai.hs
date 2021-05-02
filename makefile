HC=ghc
DHC=ghci
HFLAGS=-dynamic -threaded
PREFIX=/usr
MANPREFIX=$(PREFIX)/share/man

.PHONY: ekitai clean test

ekitai: Ekitai.hs
	$(HC) $(HFLAGS) --make $< && \
		mv Ekitai ekitai

test: Ekitai.hs
	$(DHC) $<

clean:
	rm ekitai *.o *.hi

install: ekitai
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp -f ekitai $(DESTDIR)$(PREFIX)/bin
	chmod 775 $(DESTDIR)$(PREFIX)/bin/ekitai
	mkdir -p $(DESTDIR)$(MANPREFIX)/man1
	cp -f ekitai.1 $(DESTDIR)$(MANPREFIX)/man1

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/ekitai \
		$(DESTDIR)$(MANPREFIX)/man1/ekitai.1

