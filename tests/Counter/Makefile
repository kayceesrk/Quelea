EXE=Counter
SCRIPTS:=$(shell cqlsh -f ../create_keyspace.cql)
BROKER=

all: $(EXE) $(SCRIPTS)

Counter: Counter.hs
	ghc $(BROKER) -threaded Counter.hs

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
