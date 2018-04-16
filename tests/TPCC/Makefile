EXE=TPCC
SCRIPTS:=$(shell cqlsh -f ../create_keyspace.cql)
BROKER=

all: $(EXE) $(SCRIPTS)

TPCC: TPCC1.hs
	ghc $(BROKER) -rtsopts -threaded TPCC1.hs -o $(EXE)

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
