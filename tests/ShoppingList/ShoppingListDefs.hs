{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module ShoppingListDefs (
  Operation(..),
  Quantity,
  ItemName,
  viewList, viewListCtrt,
  changeQuantity, changeQuantityCtrt,
  deleteItem, deleteItemCtrt,
  renameItem, renameItemCtrt,

  createTables, dropTables
) where

import Database.Cassandra.CQL
import Data.Serialize hiding (Put, Get)

import Codeec.Types
import Codeec.Contract
import Codeec.TH
import Codeec.DBDriver
import Data.Int (Int64)
import Control.Applicative ((<$>))
import Data.Tuple.Select (sel1)
import Data.DeriveTH
import qualified Data.Map as M
import qualified Data.Set as S
import Data.UUID
import Control.Lens
import Data.Time.Clock

type Quantity = Int
type ItemName = String

data ShoppingList = DeleteItem_ ItemName UTCTime
                  | RenameItem_ ItemName ItemName UTCTime
                  | ChangeQuantity_ ItemName Quantity UTCTime
                  | ViewList_ deriving Eq

getTimestamp :: ShoppingList -> UTCTime
getTimestamp (DeleteItem_ _ ts) = ts
getTimestamp (RenameItem_ _ _ ts) = ts
getTimestamp (ChangeQuantity_ _ _ ts) = ts
getTimestamp (ViewList_) = error "getTimeStamp: ViewList"

instance Ord ShoppingList where
  (<=) a b = (<=) (getTimestamp a) (getTimestamp b)

$(derive makeSerialize ''ShoppingList)

instance CasType ShoppingList where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Effectish ShoppingList where
  summarize effList =
    let (m,acc) =
          foldl (\(m,acc) eff ->
            case eff of
              ChangeQuantity_ n q t -> (M.insertWith (\(q1,t1) (q2,t2) -> (q1 + q2, max t1 t2)) n (q,t) m, acc)
              DeleteItem_ n t -> (M.delete n m, eff:acc)
              RenameItem_ n1 n2 t ->
                case M.lookup n1 m of
                  Nothing -> (m, eff:acc)
                  Just (q,_) -> (M.delete n1 $ M.insert n2 (q,t) m, eff:acc)
              ViewList_ -> error "summarize: ViewList_ is unexpected") (M.empty, []) effList
    in acc ++ (M.foldlWithKey (\acc n (q,t) -> (ChangeQuantity_ n q t):acc) [] m)

viewList :: [ShoppingList] -> () -> (M.Map ItemName Quantity, Maybe ShoppingList)
viewList effList _ =
  let sumList = summarize effList
  in (foldl mkMap M.empty sumList, Nothing)
  where
    mkMap m (ChangeQuantity_ n q _) | q > 0 = M.insert n q m
    mkMap m _ = m

changeQuantity :: [ShoppingList] -> (ItemName, Quantity, UTCTime) -> ((), Maybe ShoppingList)
changeQuantity _ (n,q,t) = ((), Just $ ChangeQuantity_ n q t)

deleteItem :: [ShoppingList] -> (ItemName, UTCTime) -> ((), Maybe ShoppingList)
deleteItem _ (n,ts) = ((), Just $ DeleteItem_ n ts)

renameItem :: [ShoppingList] -> (ItemName, ItemName, UTCTime) -> (Bool, Maybe ShoppingList)
renameItem effList (oldName, newName, timestamp) =
  let itemMap = sel1 $ viewList effList ()
  in if ((not $ M.member oldName itemMap) || M.member newName itemMap)
     then (False, Nothing)
     else (True, Just $ RenameItem_ oldName newName timestamp)

mkOperations [''ShoppingList]
$(derive makeSerialize ''Operation)

--------------------------------------------------------------------------------
-- Contracts

{- session 0
   ----------
   X -> 10 // initially
   Delete(X) (or) Rename(X,Y)
   NewItem(X,5)

   session 1
   ---------
   Sees x->10 //initially
   does not see delete
   sees (ChangeQuantity x 5)
   result X -> 15 //Wrong!! -}

viewListCtrt :: Contract Operation
viewListCtrt x = forallQ2_ [DeleteItem, RenameItem] [ChangeQuantity] $ \a b ->
                   forall_ $ \c -> liftProp $
                     (soo a b ∧ vis b x ⇒ vis a x) ∧
                     (soo c x ⇒ vis c x)

changeQuantityCtrt :: Contract Operation
changeQuantityCtrt x = liftProp $ true

renameItemCtrt :: Contract Operation
renameItemCtrt x = forallQ_ [RenameItem] $ \a -> liftProp $ (vis a x ∨ vis x a ∨ sameEff a x)

deleteItemCtrt :: Contract Operation
deleteItemCtrt x = liftProp $ true


--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "ShoppingList"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "ShoppingList"
