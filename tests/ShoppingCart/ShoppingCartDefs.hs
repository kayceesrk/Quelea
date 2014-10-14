{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module ShoppingCartDefs (
  ItemID(..),
  CartID(..),
  ItemEffect(..),
  Operation(..),
  createTables,
  dropTables,

  stockItem, stockItemCtrt,
  unStockItem, unStockItemCtrt,
  showItem, showItemCtrt,

  addCart, addCartCtrt,
  removeCart, removeCartCtrt,

  addItemsToCart, addItemsToCartCtrt,
  removeItemsFromCart, removeItemsFromCartCtrt,
  getItemsInCart, getItemsInCartCtrt
) where


import Database.Cassandra.CQL
import Data.Serialize as S
import qualified Data.Map as M
import Data.Time.Clock
import Data.UUID
import Control.Applicative ((<$>))
import Data.Tuple.Select (sel1)
import Data.DeriveTH

import Codeec.Types
import Codeec.Contract
import Codeec.TH
import Codeec.DBDriver
import Data.List (groupBy)

--------------------------------------------------------------------------------
-- Item table : key = ItemID

newtype ItemID = ItemID UUID deriving (Eq, Ord)

data ItemEffect = ShowItem_ 
                | StockItem_ Int{- Qty -}
                | UnStockItem_ Int{- Qty -} deriving Eq

$(derive makeSerialize ''ItemID)

$(derive makeSerialize ''ItemEffect)

instance CasType ItemEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

stockItem :: [ItemEffect] -> Int -> ((),Maybe ItemEffect)
stockItem _ qty = ((),Just $ StockItem_ qty)

unStockItem :: [ItemEffect] -> Int -> ((), Maybe ItemEffect)
unStockItem _ qty = ((),Just $ UnStockItem_ qty)

showItem :: [ItemEffect] -> () -> (Int, Maybe ItemEffect)
showItem ctxt _ = 
  let qty = foldl acc 0 ctxt
  in (qty, Just ShowItem_)
  where
    acc s (StockItem_ i) = s + i
    acc s (UnStockItem_ i) = s - i
    acc s ShowItem_ = s

instance Effectish ItemEffect where
  summarize ctxt = 
    let (v, _) = showItem ctxt ()
    in [StockItem_ v]

--------------------------------------------------------------------------------
-- Carts table : Key = CartId

newtype CartID = CartID UUID deriving (Eq, Ord)

data CartEffect = AddCart_
                | RemoveCart_ deriving Eq

$(derive makeSerialize ''CartID)

$(derive makeSerialize ''CartEffect)

instance Effectish CartEffect where
  summarize [AddCart_,RemoveCart_] = []
  summarize [RemoveCart_,AddCart_] = []
  summarize [AddCart_] = [AddCart_]
  summarize _ = error "Cart summarization encountered strange case"

instance CasType CartEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

addCart :: [CartEffect] -> () -> ((),Maybe CartEffect)
addCart _ () = ((),Just AddCart_)

removeCart :: [CartEffect] -> () -> ((),Maybe CartEffect)
removeCart _ () = ((), Just AddCart_)

--------------------------------------------------------------------------------
-- CartItems table : Key = CartID

data CartItemEffect = AddItemsToCart_ ItemID Int{- Qty -}
                    | GetItemsInCart_ 
                    | RemoveItemsFromCart_ ItemID Int{- Qty -}

$(derive makeSerialize ''CartItemEffect)

instance CasType CartItemEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

addItemsToCart :: [CartItemEffect] -> (ItemID,Int) -> ((),Maybe CartItemEffect)
addItemsToCart _ (itemID,qty) = ((),Just $ AddItemsToCart_ itemID qty)

removeItemsFromCart :: [CartItemEffect] -> (ItemID,Int) -> (Bool,Maybe CartItemEffect)
removeItemsFromCart ctxt (itemID,qty) = 
  let (items, _) = getItemsInCart ctxt () in 
  let cartQtyOp = lookup itemID items in
    case cartQtyOp of
      Nothing -> (False,Nothing)
      Just cartQty -> if cartQty >= qty
                      then (True,Just $ RemoveItemsFromCart_ itemID qty)
                      else (False,Nothing)

getItemsInCart:: [CartItemEffect] -> () -> ([(ItemID,Int)], Maybe CartItemEffect)
getItemsInCart ctxt _ = 
  let (log :: [(ItemID,Int)]) = foldr acc [] ctxt in
  let (itemGroups :: [[(ItemID,Int)]]) = (\(id,_) (id',_) ->  id == id') `groupBy` log in
  let items = map grpSummarize itemGroups in
    (items, Just GetItemsInCart_)
  where
    acc :: CartItemEffect -> [(ItemID,Int)] -> [(ItemID,Int)]
    acc (AddItemsToCart_ id qty) s = (id,qty):s
    acc (RemoveItemsFromCart_ id qty) s = (id,0-qty):s
    acc GetItemsInCart_ s = s
    grpSummarize :: [(ItemID,Int)] -> (ItemID,Int)
    grpSummarize l = (fst $ hd l, foldl (\x y -> x+y) 0 (map snd l))
    hd :: [(ItemID,Int)] -> (ItemID,Int)
    hd (x:xs) = x
    hd _ = error "hd error"

instance Effectish CartItemEffect where
  summarize ctxt = 
    let (items,_)  = getItemsInCart ctxt () in
      map (\(id,i) -> AddItemsToCart_ id i) items

--------------------------------------------------------------------------------

mkOperations [''ItemEffect, ''CartEffect, ''CartItemEffect]
$(derive makeSerialize ''Operation)

--------------------------------------------------------------------------------
-- Contracts

trueCtrt :: Contract Operation
trueCtrt x = liftProp $ true

stockItemCtrt :: Contract Operation
stockItemCtrt = trueCtrt

unStockItemCtrt :: Contract Operation
unStockItemCtrt = trueCtrt

showItemCtrt :: Contract Operation
showItemCtrt = trueCtrt

addCartCtrt :: Contract Operation
addCartCtrt = trueCtrt

removeCartCtrt :: Contract Operation
removeCartCtrt = trueCtrt

addItemsToCartCtrt :: Contract Operation
addItemsToCartCtrt = trueCtrt

removeItemsFromCartCtrt :: Contract Operation
removeItemsFromCartCtrt = trueCtrt 

getItemsInCartCtrt :: Contract Operation
getItemsInCartCtrt x = forallQ_ [AddItemsToCart, RemoveItemsFromCart] $
                          \a -> liftProp $ soo a x ⇒ vis a x



--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "ItemEffect"
  createTable "CartEffect"
  createTable "CartItemEffect"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "ItemEffect"
  dropTable "CartEffect"
  dropTable "CartItemEffect"
