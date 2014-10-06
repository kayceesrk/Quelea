{-# Language TemplateHaskell, EmptyDataDecls, ScopedTypeVariables #-}

module Codeec.TH (
  mkOperations,
  checkOp
) where


import Codeec.Types
import Codeec.Contract.Language
import Codeec.Contract.TypeCheck
import Language.Haskell.TH
import Codeec.Marshall
import Codeec.Shim
import Language.Haskell.TH.Syntax (lift)

mkOperations :: [Name] -> Q [Dec]
mkOperations l = do
  pl <- procNameList l
  let (_,consList) = unzip pl
  d1 <- dataD (return []) (mkName operationsTyConStr) [] consList [mkName "Show", mkName "Eq", mkName "Ord", mkName "Read", mkName "Enum"]
  let ap = appT ([t| OperationClass |]) (conT $ mkName operationsTyConStr)
  d2 <- instanceD (return []) ap [funD 'getObjType $ map mkGetObjType pl]
  return $ [d1,d2]
  where
    procNameList :: [Name] -> Q [(String,ConQ)]
    procNameList [] = return []
    procNameList (x:xs) = do
      TyConI (DataD _ (typeName::Name) _ constructors _) <- reify x
      let typeNameStr = nameBase typeName
      let consNameStrList = map (\ (NormalC name _) -> nameBase name) constructors
      let consList = map (\s -> normalC (mkName $ take (length s - 1) s) []) consNameStrList
      let pairList = map (\c -> (typeNameStr, c)) consList
      rest <- procNameList xs
      return $ pairList ++ rest

    mkGetObjType :: (String, ConQ) -> ClauseQ
    mkGetObjType (objType, con) = do
      NormalC conName _ <- con
      return $ Clause [ConP conName []] (NormalB (LitE (StringL objType))) []

checkOp :: OperationClass a => a -> Contract a -> ExpQ
checkOp kind c = do
  a <- classifyOperContract c $ show kind
  lift a
