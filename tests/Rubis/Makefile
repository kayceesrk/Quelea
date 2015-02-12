EXE=Rubis
BROKER=

all: $(EXE)

Rubis: Rubis.hs
	ghc $(BROKER) -threaded -rtsopts Rubis.hs

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
