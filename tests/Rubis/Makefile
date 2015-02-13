EXE=Rubis Rubis_strong
BROKER=

all: $(EXE)

Rubis: Rubis.hs RubisDefs.hs RubisTxns.hs
	ghc $(BROKER) -threaded -rtsopts Rubis.hs

Rubis_strong: Rubis_strong.hs RubisDefs.hs RubisTxns.hs
	ghc $(BROKER) -threaded -rtsopts Rubis_strong.hs


clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
