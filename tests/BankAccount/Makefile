EXE=BankAccount_Q BankAccount_SC BankAccount_CC BankAccount_EC BankAccount_1key
BROKER=

all: $(EXE)

BankAccount_Q: BankAccount_Q.hs
	ghc $(BROKER) -rtsopts -threaded BankAccount_Q.hs

BankAccount_EC: BankAccount_EC.hs
	ghc $(BROKER) -threaded BankAccount_EC.hs

BankAccount_CC: BankAccount_CC.hs
	ghc $(BROKER) -threaded BankAccount_CC.hs

BankAccount_SC: BankAccount_SC.hs
	ghc $(BROKER) -threaded BankAccount_SC.hs

BankAccount_1key: BankAccount_1key.hs
	ghc $(BROKER) -threaded BankAccount_1key.hs

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
