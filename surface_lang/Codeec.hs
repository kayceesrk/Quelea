{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, BangPatterns, EmptyDataDecls #-}

module Codeec (
-- Types
---------

EC,
Proc,

Effect,
Ctxt,
Key,

Table,

createTable,
performOp,

)
where

{-
 * TODO
 * ----
 * (1) A way to describe the store (tables) -- Essentially a datatype
 * definition whose values represent the write performed on the table.
 *
 * (2) A way to describe operations on the store (Stored procedures)
 * (2.1) The procedures are restricted to single-key.
 * (2.2) Can perform effects on the store.
 *
 * (3) A way to describe the proedure-level consistency.
 *
 * (4) A wat to describe global consistency.
 *
 * Tie together (3) and (4) with (1) and (2).
 -}

import qualified Database.Cassandra.CQL as CQL
import Data.Graph.Inductive.Graph
import SpecCheck

data EC a
data Proc a b
class CQL.CasType a => Effect a
data Ctxt a = Gr a ()

data Key
data Table

createTable :: Table -> EC ()
createTable = undefined

effect :: (Effect a) => a -> Proc a ()
effect = undefined

performOp :: Effect a
          => (Ctxt a -> arg -> Proc a res)
          -> Table
          -> Key
          -> arg
          -> EC res
performOp = undefined
