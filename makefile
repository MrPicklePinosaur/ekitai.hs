HC=ghc
DHC=ghci
HFLAGS=-dynamic -threaded
PREFIX=/usr
SAMPLEPREFIX=$(PREFIX)/share
MANPREFIX=$(PREFIX)/share/man

.PHONY: ekitai clean test install uninstall

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
	mkdir -p $(DESTDIR)$(SAMPLEPREFIX)/ekitai
	cp -r samples $(DESTDIR)$(SAMPLEPREFIX)/ekitai

uninstall:
	rm -rf $(DESTDIR)$(PREFIX)/bin/ekitai \
		$(DESTDIR)$(MANPREFIX)/man1/ekitai.1 \
		$(DESTDIR)$(SAMPLEPREFIX)/ekitai

