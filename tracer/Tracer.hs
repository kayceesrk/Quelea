{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Tracer where

import Language.Haskell.TH
import Z3.Monad

newtype EventType = EventType { unEventType :: Z3 Sort}

createZ3EventType :: Name -> IO (EventType)
createZ3EventType t = do
  TyConI (DataD _ (typeName::Name) _ constructors _) <- runQ $ reify t
  let empty :: Z3 [Constructor] = return []
  let makeCons consStr = do
      consSym <- mkStringSymbol consStr
      isConsSym <- mkStringSymbol $ "is_" ++ consStr
      mkConstructor consSym isConsSym []
  let bar (acc :: Z3 [Constructor]) (NormalC name _) = do
      consList <- acc
      newCons <- makeCons $ nameBase name
      return (newCons:consList)
  let makeDatatype = do
      consList <- foldl bar empty constructors
      dtSym <- mkStringSymbol (nameBase typeName)
      mkDatatype dtSym consList
  return $ EventType makeDatatype
