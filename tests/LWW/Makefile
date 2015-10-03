EXE=LWW_simple LWW_txn LWW_1key
SCRIPTS:=$(shell cqlsh -f ../create_keyspace.cql)
BROKER=

all: $(EXE) $(SCRIPTS)

LWW_simple: LWW_simple.hs
	ghc $(BROKER) -threaded LWW_simple.hs

LWW_txn: LWW_txn.hs
	ghc $(BROKER) -threaded LWW_txn.hs

LWW_1key: LWW_1key.hs
	ghc $(BROKER) -threaded -rtsopts LWW_1key.hs

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
