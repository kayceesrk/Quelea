{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DoAndIfThenElse #-}

module Codeec.Contract.RequiredDependencies (
  calcReqDeps
) where

import Codeec.Types
import Codeec.Contract.Language
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Control.Lens
import Data.Maybe (fromJust)

data AnalState a = AnalState {
  _effKindMap :: M.Map Effect [a],
  _depMap     :: M.Map a (S.Set a)
}

makeLenses ''AnalState

type AnalM a = State (AnalState a) ()

procFol :: OperationClass a => Fol a -> AnalM a
procFol (Forall effs fol) = do
  ekm <- use effKindMap
  let newEff = Effect $ M.size ekm + 1
  effKindMap .= M.insert newEff effs ekm
  procFol $ fol newEff
procFol (Plain prop) = procProp prop

procProp :: OperationClass a => Prop a -> AnalM a
procProp PTrue = return ()
procProp (Not p) = procProp p
procProp (AppRel rel e1 e2) = do
  let e0 = Effect 0
  if hasVis rel && e1 /= e0 && e2 /= e0
  then do
    ekm <- use effKindMap
    let keyList = fromJust $ M.lookup e2 ekm
    let valList = fromJust $ M.lookup e1 ekm
    mapM (\k -> mapM (insertIntoDepMap k) valList) keyList
    return ()
  else return ()
  where
    insertIntoDepMap k v = do
      dm <- use depMap
      depMap .= M.insertWith S.union k (S.singleton v) dm
procProp (Conj p1 p2) = procProp p1 >> procProp p2
procProp (Disj p1 p2) = procProp p1 >> procProp p2
procProp (Impl p1 p2) = procProp p1 >> procProp p2
procProp _ = error "procProp : unexpected token"

hasVis :: Rel -> Bool
hasVis Vis = True
hasVis (TC r) = hasVis r
hasVis (Union r1 r2) = hasVis r1 || hasVis r2
hasVis (Intersect r1 r2) = hasVis r1 || hasVis r2
hasVis _ = False

calcReqDeps :: OperationClass a => [Contract a] -> M.Map a (S.Set a)
calcReqDeps [] = M.empty
calcReqDeps (ctrt:tl) =
  let dm = calcReqDeps tl
      ekm = M.empty
      initState = AnalState ekm dm
      finalState = execState (procFol $ ctrt $ Effect 0) initState
  in finalState^.depMap
