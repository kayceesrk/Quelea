{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer hiding (inActSoup)
import qualified Tracer as T
import Language.Haskell.TH

data Event = Put | Get deriving Show
instance SolverEvent Event

data Attr = Key | Value deriving Show
instance SolverAttr Attr

tabIO s = doIO $ (putStr "\t") >> s

inActSoup :: [Action] -> Prop
inActSoup [] = true_
inActSoup (x:xs) = (T.inActSoup x) `and_` inActSoup xs

main = do
  let readsFrom = forall_ $ \a -> exists_ $ \b -> prop $
                    (isEventOf a Get) `implies_`
                    (isEventOf b Put `and_` sameAttr Key a b `and_`
                    sameAttr Value a b `and_` visTo b a)
  -----------------------------------------------------------------------------
  -- Test 1
  --
  -- Expect Sat
  putStrLn $ "Test 1: Expect Sat"
  let test = do
        r <- checkConsistency $ prop false_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) test

  -----------------------------------------------------------------------------
  -- Test 2
  --
  putStrLn $ "Test 2"

  let test = do
        s <- newSession "S"
        a <- newAction "a" Put [(Key,"x"), (Value,"1")] basicEventual s
        b <- newAction "b" Get [(Key,"x"), (Value,"1")] basicEventual s

        doIO $ putStrLn "(1) Expect Unsat"
        r <- checkConsistency $ prop $ sameAttr Key a b
        tabIO $ print r

        doIO $ putStrLn "(2) Expect Unsat"
        r <- checkConsistency $ prop $ sameAttr Value a b
        tabIO $ print r

        doIO $ putStrLn "(3) Expect Sat"
        r <- checkConsistency $ exists_ $ \c -> prop $
          (inActSoup [c] `and_` distinctActs [a,b,c])
        tabIO $ print r

        c <- newAction "c" Put [(Key,"x"), (Value,"2")] basicEventual s
        d <- newAction "d" Get [(Key,"x"), (Value,"2")] basicEventual s

        doIO $ putStrLn "(4) Expect Sat"
        r <- addAssertion readsFrom
        tabIO $ print r

        doIO $ putStrLn "(5) Expect Sat"
        r <- checkConsistency{-AndIfFailDo showModel -} $ prop false_
        tabIO $ print r

  runECD $(liftEvent ''Event) $(liftAttr ''Attr) test

  -----------------------------------------------------------------------------
  -- Test 3
  --
  putStrLn $ "Test 3"

  let test = do
        s <- newSession "S"
        w0 <- newInitWrite "w0" Put [(Key,"x"), (Value,"2")]
        a <- newAction "a" Put [(Key,"x"), (Value,"1")] basicEventual s
        b <- newAction "b" Get [(Key,"x"), (Value,"2")] basicEventual s

        doIO $ putStrLn "(1) Expect Sat"
        r <- addAssertion readsFrom
        tabIO $ print r

        doIO $ putStrLn "(2) Expect Sat"
        r <- checkConsistency{-AndIfFailDo showModel -} $ prop false_
        tabIO $ print r

  runECD $(liftEvent ''Event) $(liftAttr ''Attr) test

  -----------------------------------------------------------------------------
  -- Test 4
  --
  putStrLn $ "Test 4"

  let test = do
        s1 <- newSession "s1"
        a <- newAction "a" Get [(Key,"x"), (Value,"1")] basicEventual s1
        b <- newAction "b" Put [(Key,"y"), (Value,"1")] basicEventual s1

        s2 <- newSession "s2"
        c <- newAction "c" Get [(Key,"y"), (Value,"1")] basicEventual s2
        d <- newAction "d" Put [(Key,"x"), (Value,"1")] basicEventual s2

        doIO $ putStrLn "(1) Expect Sat"
        r <- addAssertion readsFrom
        tabIO $ print r

        -- In this example, the only possible assignment for the visibility
        -- relation creates a cycle. ThinAir axiom fails. Hence the execution
        -- is impossible.
        doIO $ putStrLn "(2) Expect ExecImpossible"
        r <- checkConsistencyAndIfFailDo showModel $ prop false_
        tabIO $ print r

  runECD $(liftEvent ''Event) $(liftAttr ''Attr) test

