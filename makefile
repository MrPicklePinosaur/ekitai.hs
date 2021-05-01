
HC=ghc
DHC=ghci
HFLAGS=-dynamic -threaded

.PHONY: ekitai clean test

ekitai: Ekitai.hs
	$(HC) $(HFLAGS) --make $<

test: Ekitai.hs
	$(DHC) $<

clean:
	rm Ekitai *.o *.hi
