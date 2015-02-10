EXE=LWW_simple LWW_txn LWW_1key

all: $(EXE)

LWW_simple: LWW_simple.hs
	ghc -threaded LWW_simple.hs

LWW_txn: LWW_txn.hs
	ghc -threaded LWW_txn.hs

LWW_1key: LWW_1key.hs
	ghc -threaded -rtsopts LWW_1key.hs


clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
