EXE=ShoppingCart
SCRIPTS:=$(shell cqlsh -f ../create_keyspace.cql)
BROKER=

all: $(EXE) $(SCRIPTS)

ShoppingCart: ShoppingCart1.hs
	ghc $(BROKER) -threaded ShoppingCart1.hs -o $(EXE)

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
