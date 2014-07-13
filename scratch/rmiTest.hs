{-# LANGUAGE ScopedTypeVariables #-}

import Database.Cassandra.CQL as CQL
import qualified Data.Map as Map
import Data.Serialize as S
import Data.Word (Word8)
import Control.Applicative

-- CQL.CasType a - those types of values which can be store in cassandra.
-- Serialize a - those types of values which can be sent over the network.
data BAEffect = Deposit Int | Withdraw Int deriving Show

instance Serialize BAEffect where
  put (Deposit v) = putTwoOf S.put S.put (0::Word8, v)
  put (Withdraw v) = putTwoOf S.put S.put (1::Word8, v)
  get = do
    (i::Word8,v::Int) <- getTwoOf S.get S.get
    case i of
      0 -> return $ Deposit v
      1 -> return $ Withdraw v

instance CQL.CasType BAEffect where
  getCas = do
    r <- decode . unBlob <$> getCas
    case r of
      Left _ -> error "Parse fail"
      Right v -> return $ v
  putCas = putCas . Blob . encode
  casType _ = CBlob

type Res a = (a, Maybe BAEffect)

deposit :: [BAEffect] -> Int -> Res ()
deposit _ amt = ((), Just $ Deposit amt)

withdraw :: [BAEffect] -> Int -> Res Bool
withdraw ctxt amt =
  let (bal, _) = getBalance ctxt ()
  in if bal > amt
     then (True, Just $ Withdraw amt)
     else (False, Nothing)

getBalance :: [BAEffect] -> () -> Res Int
getBalance ops () =
  let v = foldl acc 0 ops
  in (v, Nothing)
  where
    acc s (Deposit i) = s + i
    acc s (Withdraw i) = s - i

mkGen :: (CasType a, Serialize arg, Serialize res)
      => ([a] -> arg -> (res, Maybe a))
      -> ([ByteString] -> ByteString -> (ByteString, Maybe ByteString))

-- We want the effects to be storable, while the argument and results to be
-- serializable.
functionMap :: (CasType a, Serialize arg, Serialize res)
            => Map.Map String ([a] -> arg -> (res, Maybe a))
functionMap = Map.fromList [("deposit",deposit),
                            ("withdraw",withdraw),
                            ("getBalance",getBalance)]

main :: IO ()
main = undefined
