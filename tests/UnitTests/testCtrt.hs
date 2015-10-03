{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

import Control.Applicative ((<$>))

import Quelea.Types
import Quelea.Contract
import Quelea.TH

import TestCtrtDefs

main = do
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt sc) ⇒ (Raw $ fol2Z3Ctrt sc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt sc) ⇒ (Raw $ fol2Z3Ctrt cc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt sc) ⇒ (Raw $ fol2Z3Ctrt cv)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt cc) ⇒ (Raw $ fol2Z3Ctrt sc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect False, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt cc) ⇒ (Raw $ fol2Z3Ctrt cc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt cc) ⇒ (Raw $ fol2Z3Ctrt cv)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt cv) ⇒ (Raw $ fol2Z3Ctrt sc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect False, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt cv) ⇒ (Raw $ fol2Z3Ctrt cc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect False, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt cv) ⇒ (Raw $ fol2Z3Ctrt cv)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r

  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt psi) ⇒ (Raw $ fol2Z3Ctrt psi)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt psi) ⇒ (Raw $ fol2Z3Ctrt mav)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt psi) ⇒ (Raw $ fol2Z3Ctrt rc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt mav) ⇒ (Raw $ fol2Z3Ctrt psi)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect False, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt mav) ⇒ (Raw $ fol2Z3Ctrt mav)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt mav) ⇒ (Raw $ fol2Z3Ctrt rc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt rc) ⇒ (Raw $ fol2Z3Ctrt psi)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect False, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt rc) ⇒ (Raw $ fol2Z3Ctrt mav)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect False, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt rc) ⇒ (Raw $ fol2Z3Ctrt rc)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect True, Result=" ++ show r

  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt psi) ⇒ (Raw $ fol2Z3Ctrt psiFlipped)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect False, Result=" ++ show r
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt rc) ⇒ (Raw $ fol2Z3Ctrt psiFlipped)
  let f :: Fol () = liftProp $ (Raw $ fol2Z3Ctrt mav) ⇒ (Raw $ fol2Z3Ctrt psiFlipped)
  isValid "Test1" f >>= \r -> putStrLn $ "Expect False, Result=" ++ show r

  res <- underMonotonicAtomicView mav dummyZ3Sort
  putStrLn $ "Is mav under MAV? " ++ show res

  res <- underMonotonicAtomicView psiFlipped dummyZ3Sort
  putStrLn $ "Is psiFlipped under MAV? " ++ show res

  -- putStrLn . show $ $(checkTxn "Test1" psiFlipped)
