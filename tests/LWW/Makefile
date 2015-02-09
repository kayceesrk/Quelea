EXE=LWW_simple LWW_txn

all: LWW_simple LWW_txn

LWW_simple: LWW_simple.hs
	ghc -threaded LWW_simple.hs

LWW_txn: LWW_txn.hs
	ghc -threaded LWW_txn.hs

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
