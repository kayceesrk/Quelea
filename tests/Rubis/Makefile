EXE=Rubis

all: $(EXE)

Rubis: Rubis.hs
	ghc -threaded -rtsopts Rubis.hs

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
