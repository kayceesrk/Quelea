{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer
import Language.Haskell.TH

data Event = NewEmp | DelEmp | NewAlias
           | NewDep | DelDep
           | IncSal | DecSal | CalcSal

data Attr = Dept | Alias | Empl

tabIO s = doIO $ (putStr "\t") >> s

main = do

 -- HIGH LEVEL
 -- ----------
 -- Employee's alias is unique.
 let uniqueAlias = forall_ $ \a -> forall_ $ \b -> prop $
                ((isEvent a [| NewEmp |]) `and_`
                (isEvent b [| NewEmp |]) `and_`
                (not_ $ sameAct a b)) `implies_`
                (not_ $ sameAttr [| Alias |] a b)

 -- HIGH LEVEL
 -- ----------
 -- Decrease salary operations are strongly consistent.
 let totDecSal = forall_ $ \a -> forall_ $ \b -> prop $
                  ((isEvent a [| DecSal |]) `and_`
                  (isEvent a [| DecSal |]) `and_`
                  (not_ $ sameAct a b)) `implies_`
                  ((a `visTo` b) `or_` (b `visTo` a))

 -- HIGH LEVEL
 -- ----------
 -- For salary calculation on an employee, the employee's department
 -- information is available.
 ---
 -- Assume
 -- ------
 -- With concurrent insert and delete on the same key, insert wins!
 --
 -- LOW LEVEL
 -- ---------
 -- For any action a and b, such that event(a) = NewEmp, event(b) = CalcSal,
 -- Empl(b) = key(a) and a --vis--> b, there exists an action c with event(c) =
 -- NewDept, key(c) = dept(a), c --vis--> b, and there does not exists an
 -- action d with event(d) = DelDep, and c --vis--> d.
 --
 let calcSalSafety = forall_ $ \a -> forall_ $ \b -> exists_ $ \c -> exists_ $ \d -> prop $
                      ((isEvent a [| NewEmp |]) `and_`
                      (isEvent b [| CalcSal |]) `and_`
                      (a `visTo` b) `and_` (keyRelAttr a b [| Empl |])) `implies_`
                      ((isEvent c [| NewDep |]) `and_`
                       (keyRelAttr c a [| Dept |] `and_`
                       (c `visTo` b) `and_`
                       (not_ $ (isEvent d [| DelDep |]) `and_`
                               (c `visTo` d))))

 let test = do
       res <- checkConsistency uniqueAlias
       tabIO $ print res
       res <- checkConsistency totDecSal
       tabIO $ print res
       res <- checkConsistency calcSalSafety
       tabIO $ print res

 runECD $(liftEvent ''Event) test
