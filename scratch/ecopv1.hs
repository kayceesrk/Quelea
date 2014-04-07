{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

class Show a => Event a
class Show a => Attr a

data Action
data FOL = FOL
data Ctxt

data ECOP a b where
  ECOP :: (Event a, Attr b) => {guard :: (Action -> FOL),
                                body  :: Ctxt -> c -> (d, a, [(b,String)])}
                            -> ECOP a b

data BA_Event = Deposit | Withdraw deriving Show
data BA_Attr = Value deriving Show

instance Event BA_Event
instance Attr BA_Attr

foo :: [ECOP BA_Event BA_Attr] = []

depositGuard :: Action -> FOL
depositGuard _ = FOL

depositBody :: Ctxt -> Int -> ((), BA_Event, [(BA_Attr, String)])
depositBody _ amount = ((), Deposit, [(Value, show amount)])

withdrawGuard :: Action -> FOL
withdrawGuard _ = FOL

withdrawBody :: Ctxt -> Int -> (Bool, BA_Event, [(BA_Attr, String)])
withdrawBody _ amount = (False, Deposit, [(Value, show amount)])

bar :: [ECOP BA_Event BA_Attr] = [ECOP depositGuard depositBody]
bar2 :: [ECOP BA_Event BA_Attr] = [ECOP withdrawGuard withdrawBody]

bar3 = bar2 ++ bar

main = return ()


data BankAccount = Deposit Int | Withdraw Int

data Exec upd

deposit     :: Ctxt upd -> key -> arg -> ECOP upd res
withdraw    ::
getBalance  ::


performOp :: (Ctxt upd -> arg -> ECOP upd res) -> Table -> key -> arg -> IO res

performOp deopsit ''BankAccount acctNo amount

class Monad (ECOP upd) => ECOP upd res
