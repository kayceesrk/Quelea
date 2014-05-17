module Types
(
  Key,
  Sess,
  Index,
  Addr(..),
  Z3Row(..),
  Z3Ctxt
) where

import Data.UUID
import Data.Int (Int64)
import qualified Data.Set as S

type Key  = UUID
type Sess = UUID
type Index = Int64
data Addr = Addr Sess Index deriving (Eq, Ord, Read, Show)

data Z3Row = Z3Row {
                 _sess_r4z3 :: Sess,
                 _idx_r4z3  :: Index,
                 _vis_r4z3  :: S.Set Addr
               } deriving Eq

instance Ord Z3Row where
  compare r1 r2 =
    let Z3Row s1 a1 _ = r1
        Z3Row s2 a2 _ = r2
    in if (s1 < s2) then LT
       else if (s1 > s2) then GT
       else if (a1 < a2) then LT
       else if (a1 > a2) then GT
       else EQ

type Z3Ctxt = [Z3Row]
