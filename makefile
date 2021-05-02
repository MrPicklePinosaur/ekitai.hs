
HC=ghc
DHC=ghci
HFLAGS=-dynamic -threaded

.PHONY: ekitai clean test

ekitai: Ekitai.hs
	$(HC) $(HFLAGS) --make $< && \
		mv Ekitai ekitai

test: Ekitai.hs
	$(DHC) $<

clean:
	rm ekitai *.o *.hi
