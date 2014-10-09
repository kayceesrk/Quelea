{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DoAndIfThenElse #-}

module Codeec.Contract.TypeCheck (
  classifyOperContract,
  classifyTxnContract,
  isValid,
  isSat,
  fol2Z3Ctrt
) where


import Codeec.Types
import Codeec.Contract.Language
import Z3.Monad hiding (mkFreshFuncDecl, mkFreshConst, assertCnstr, push, pop,
                        check, getModel)
import qualified Z3.Monad as Z3M (mkFreshFuncDecl, mkFreshConst, assertCnstr,
                                  push, pop, check, getModel)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH
import Control.Lens
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (find)
import Control.Applicative ((<$>))
import System.IO

-------------------------------------------------------------------------------
-- Types

makeLenses ''Z3CtrtState

-------------------------------------------------------------------------------
-- Helper

-- #define DEBUG_SHOW
-- #define DEBUG_CHECK
-- #define DEBUG_SANITY

check :: Z3 Result
#ifdef DEBUG_SHOW
check = do
  liftIO $ do
    putStrLn "(check-sat)"
    hFlush stdout
    hFlush stderr
  Z3M.check
#else
check = Z3M.check
#endif

getModel :: Z3 (Result, Maybe Model)
#ifdef DEBUG_SHOW
getModel = do
  liftIO $ do
    putStrLn "(check-sat) ;; get-model"
    hFlush stdout
    hFlush stderr
  Z3M.getModel
#else
getModel = Z3M.getModel
#endif


push :: Z3 ()
#ifdef DEBUG_SHOW
push = do
  liftIO $ do
    putStrLn "(push)"
    hFlush stdout
    hFlush stderr
  Z3M.push
#else
push = Z3M.push
#endif

pop :: Int -> Z3 ()
#ifdef DEBUG_SHOW
pop n | n /= 1 = error "pop"
pop 1 = do
  liftIO $ do
    putStrLn "(pop)"
    hFlush stdout
    hFlush stderr
  Z3M.pop 1
#else
pop = Z3M.pop
#endif

assertCnstr :: String -> AST -> Z3 ()
#ifdef DEBUG_SHOW
assertCnstr name ast = do
  setASTPrintMode Z3_PRINT_SMTLIB2_COMPLIANT
  astStr <- astToString ast
  liftIO $ do
    putStrLn $ ";; --------------------------------"
    putStrLn $ ";; Assert: " ++ name
    putStrLn $ "(assert " ++ astStr ++ ")"
    hFlush stdout
    hFlush stderr
  Z3M.assertCnstr ast
  #ifdef DEBUG_CHECK
  push
  r <- check
  liftIO $ putStrLn $ ";; Assert Result: " ++ (show r)
  pop 1
  #endif
#else
assertCnstr s a = Z3M.assertCnstr a
#endif

mkFreshFuncDecl :: String -> [Sort] -> Sort -> Z3 FuncDecl
#ifdef DEBUG_SHOW
mkFreshFuncDecl s args res = do
  setASTPrintMode Z3_PRINT_SMTLIB2_COMPLIANT
  fd <- Z3M.mkFreshFuncDecl s args res
  fdStr <- funcDeclToString fd
  liftIO $ putStrLn $ ";; --------------------------------\n" ++ fdStr ++ "\n"
  liftIO $ hFlush stdout
  liftIO $ hFlush stderr
  return fd
#else
mkFreshFuncDecl = Z3M.mkFreshFuncDecl
#endif

mkFreshConst :: String -> Sort -> Z3 AST
#ifdef DEBUG_SHOW
mkFreshConst str srt = do
  c <- Z3M.mkFreshConst str srt
  cstr <- astToString c
  srtstr <- sortToString srt
  liftIO $ putStrLn $ ";; --------------------------------"
  liftIO $ putStrLn $ "(declare-const " ++ cstr ++ " " ++ srtstr ++ ")\n"
  liftIO $ hFlush stdout
  liftIO $ hFlush stderr
  return c
#else
mkFreshConst = Z3M.mkFreshConst
#endif

#ifdef DEBUG_SANITY
debugCheck str =
  lift $ push >> check >>= (\r -> when (r == Unsat) $ error str) >> pop 1
#else
debugCheck str = return ()
#endif

lookupEff :: Effect -> StateT Z3CtrtState Z3 AST
lookupEff e = do
  em <- use effMap
  return $ fromJust $ em ^.at e

newEff :: StateT Z3CtrtState Z3 (Effect, App)
newEff = do
  em <- use effMap
  let newEff = Effect $ M.size em
  es <- use effSort
  qvConst <- lift $ mkFreshConst "E" es
  qv <- lift $ toApp qvConst
  let newEm = at newEff .~ Just qvConst $ em
  effMap .= newEm
  return $ (newEff, qv)

assertProp :: String -> Z3Ctrt -> StateT Z3CtrtState Z3 AST
assertProp str (Z3Ctrt m) = do
  ast <- m
  lift $ assertCnstr str ast
  asl <- use assertions
  assertions .= ast:asl
  return ast

-- Create a Z3 Event Sort that mirrors the Haskell Oper Type.
mkMkZ3OperSort :: Q (Z3 Sort)
mkMkZ3OperSort = do
  TyConI (DataD _ (typeName::Name) _ constructors _) <- reify $ mkName operationsTyConStr
  let typeNameStr = nameBase typeName
  let consNameStrList = map (\ (NormalC name _) -> nameBase name) constructors
  let makeCons consStr = do
      consSym <- mkStringSymbol consStr
      isConsSym <- mkStringSymbol $ "is_" ++ consStr
      mkConstructor consSym isConsSym []
  let makeDatatype = do
      consList <- sequence $ map makeCons consNameStrList
      dtSym <- mkStringSymbol typeNameStr
      mkDatatype dtSym consList
  return makeDatatype

dummyZ3Sort :: Z3 Sort
dummyZ3Sort = do
  let makeCons consStr = do
      consSym <- mkStringSymbol consStr
      isConsSym <- mkStringSymbol $ "is_" ++ consStr
      mkConstructor consSym isConsSym []
  let makeDatatype = do
      consList <- sequence $ map makeCons ["DummyCon"]
      dtSym <- mkStringSymbol "DummyTyCon"
      mkDatatype dtSym consList
  makeDatatype


-------------------------------------------------------------------------------
-- Contract to Z3 translation

-- #define EXISTS

rel2Z3Ctrt :: Rel -> Effect -> Effect -> Z3Ctrt
rel2Z3Ctrt r e1 e2 = Z3Ctrt $ do
  case r of
    Vis -> mkApp1 visRel e1 e2
    So -> mkApp1 soRel e1 e2
    SameObj -> mkApp1 sameobjRel e1 e2
    SameTxn -> mkApp1 sameTxnRel e1 e2
    SameEff -> do
      a1 <- lookupEff e1
      a2 <- lookupEff e2
      lift $ mkEq a1 a2
    Union r1 r2 -> do
      a1 <- unZ3Ctrt $ rel2Z3Ctrt r1 e1 e2
      a2 <- unZ3Ctrt $ rel2Z3Ctrt r2 e1 e2
      lift $ mkOr [a1, a2]
    Intersect r1 r2 -> do
      a1 <- unZ3Ctrt $ rel2Z3Ctrt r1 e1 e2
      a2 <- unZ3Ctrt $ rel2Z3Ctrt r2 e1 e2
      lift $ mkAnd [a1, a2]
    TC r -> do
      rm <- use tcRelMap
      case rm ^.at r of
        Nothing -> do
          es <- use effSort
          bs <- lift $ mkBoolSort
          newR <- lift $ mkFreshFuncDecl "TC" [es,es] bs
          -- Insert into rm
          let newRm = at r .~ Just newR $ rm
          tcRelMap .= newRm
          -- Prop 1
          let f1 :: Fol () = forall2_ $ \a b -> liftProp $
                  (Raw $ rel2Z3Ctrt r a b) ⇒ (Raw . Z3Ctrt $ mkApp2 newR a b)
          p1 <- unZ3Ctrt $ fol2Z3Ctrt f1
          -- Prop 2
          let f2 :: Fol () = forall3_ $ \a b c -> liftProp $
                  ((Raw . Z3Ctrt $ mkApp2 newR a b) ∧ (Raw .Z3Ctrt $ mkApp2 newR b c)) ⇒
                  (Raw . Z3Ctrt $ mkApp2 newR a c)
          p2 <- unZ3Ctrt $ fol2Z3Ctrt f2
#ifdef EXISTS
          -- Prop 3
          let f3 :: Fol () = forall2_ $ \a b -> liftProp $
                (Raw . Z3Ctrt $ mkApp2 newR a b) ⇒ ((Raw $ rel2Z3Ctrt r a b) ∨
                (exists $ \c -> (Raw $ rel2Z3Ctrt r a b) ∧ (Raw . Z3Ctrt $ mkApp2 newR b c)))
          p3 <- unZ3Ctrt $ fol2Z3Ctrt f3
          assertProp "TC" $ Z3Ctrt . lift $ mkAnd [p1,p2,p3]
#else
          assertProp "TC" $ Z3Ctrt . lift $ mkAnd [p1,p2]
#endif
          mkApp2 newR e1 e2
        Just savedR -> mkApp2 savedR e1 e2
  where
    mkApp1 idx e1 e2 = do
      r <- use idx
      mkApp2 r e1 e2
    mkApp2 r e1 e2 = do
      a1 <- lookupEff e1
      a2 <- lookupEff e2
      lift $ mkApp r [a1,a2]

prop2Z3Ctrt :: OperationClass a => Prop a -> Z3Ctrt
prop2Z3Ctrt PTrue = Z3Ctrt $ lift mkTrue
prop2Z3Ctrt (AppRel r e1 e2) = rel2Z3Ctrt r e1 e2
prop2Z3Ctrt (CurTxn e) = Z3Ctrt $ do
  ctRel <- use curTxnRel
  effAST <- lookupEff e
  lift $ mkApp ctRel [effAST]
prop2Z3Ctrt (Conj p1 p2) = Z3Ctrt $ do
  a1 <- unZ3Ctrt $ prop2Z3Ctrt p1
  a2 <- unZ3Ctrt $ prop2Z3Ctrt p2
  lift $ mkAnd [a1,a2]
prop2Z3Ctrt (Disj p1 p2) = Z3Ctrt $ do
  a1 <- unZ3Ctrt $ prop2Z3Ctrt p1
  a2 <- unZ3Ctrt $ prop2Z3Ctrt p2
  lift $ mkOr [a1,a2]
prop2Z3Ctrt (Impl p1 p2) = Z3Ctrt $ do
  a1 <- unZ3Ctrt $ prop2Z3Ctrt p1
  a2 <- unZ3Ctrt $ prop2Z3Ctrt p2
  lift $ mkImplies a1 a2
prop2Z3Ctrt (Oper eff operName) = Z3Ctrt $ do
  effAST <- lookupEff eff
  operRel <- use operRel
  operNameAST <- getZ3Operation operName
  a1 <- lift $ mkApp operRel [effAST]
  lift $ mkEq a1 operNameAST
  where
    getZ3Operation operName = do
      operSort <- use operSort
      constructors <- lift $ getDatatypeSortConstructors operSort
      nameList <- lift $ mapM getDeclName constructors
      strList <- lift $ mapM getSymbolString nameList
      let pList = zip strList constructors
      let Just (_,constructor) = find (\ (s,_) -> (show operName) == s) pList
      lift $ mkApp constructor []
prop2Z3Ctrt (Raw c) = c
prop2Z3Ctrt (Not p) = Z3Ctrt $ do
  a <- unZ3Ctrt $ prop2Z3Ctrt p
  lift $ mkNot a

fol2Z3Ctrt :: OperationClass a => Fol a -> Z3Ctrt
fol2Z3Ctrt (Plain p) = prop2Z3Ctrt p
fol2Z3Ctrt (Forall operNameList f) = Z3Ctrt $ do
  (effInt, effApp) <- newEff
  body <- unZ3Ctrt . fol2Z3Ctrt $ f effInt
  if length operNameList == 0 then
    lift $ mkForallConst [] [effApp] body
  else do
    l <- mapM (\on -> unZ3Ctrt . prop2Z3Ctrt $ Oper effInt on) operNameList
    ante <- lift $ mkOr l
    body2 <- lift $ mkImplies ante body
    lift $ mkForallConst [] [effApp] body2

exists :: OperationClass a => (Effect -> Prop a) -> Prop a
exists f = Raw $ Z3Ctrt $ do
  (effInt, effApp) <- newEff
  body <- unZ3Ctrt . prop2Z3Ctrt $ f effInt
  lift $ mkExistsConst [] [effApp] body

-------------------------------------------------------------------------------
-- Type checking helper

assertBasicAxioms :: StateT Z3CtrtState Z3 ()
assertBasicAxioms = do
  assertProp "ThinAir" $ fol2Z3Ctrt thinAir
  assertProp "doVis" $ fol2Z3Ctrt doVis
  assertProp "soTrans" $ fol2Z3Ctrt $ transRel so

  assertProp "reflRelSameTxn" $ fol2Z3Ctrt $ reflRel sameTxn
  assertProp "symRelSameTxn" $ fol2Z3Ctrt $ symRel sameTxn
  assertProp "transRelSameTxn" $ fol2Z3Ctrt $ transRel sameTxn
  assertProp "txnFollowsSess" $ fol2Z3Ctrt $ txnFollowsSess

  assertProp "reflRelSameObj" $ fol2Z3Ctrt $ reflRel sameObj
  assertProp "symRelSameObj" $ fol2Z3Ctrt $ symRel sameObj
  assertProp "transRelSameObj" $ fol2Z3Ctrt $ transRel sameObj

  assertProp "curTxnSameTxn" $ fol2Z3Ctrt $ curTxnSameTxn
  return ()
  where
    curTxnSameTxn :: Fol () = forall2_ $ \x y -> liftProp $ CurTxn x ∧ CurTxn y ⇒ sameTxn x y
    thinAir :: Fol () = forall_ $ \x -> liftProp . Not $ hb x x
    doVis :: Fol () = forall2_ $ \a b -> liftProp $ vis a b ⇒ appRel SameObj a b
    txnFollowsSess :: Fol () = forall2_ $ \a b -> liftProp $ sameTxn a b ⇒  (so a b ∨ so b a ∨ sameEff a b)

    reflRel :: (Effect -> Effect -> Prop ()) -> Fol ()
    reflRel rel = forall_ $ \x -> liftProp $ rel x x

    symRel :: (Effect -> Effect -> Prop ()) -> Fol ()
    symRel rel = forall2_ $ \a b -> liftProp $ rel a b ⇒  rel b a

    transRel :: (Effect -> Effect -> Prop ()) -> Fol ()
    transRel rel = forall3_ $ \a b c -> liftProp $ rel a b ∧ rel b c ⇒  rel a c


mkZ3CtrtState :: Sort -> Z3 Z3CtrtState
mkZ3CtrtState operSort = do
  effSort <- mkUninterpretedSort =<< mkStringSymbol "Effect"
  boolSort <- mkBoolSort

  visRel <- mkFreshFuncDecl "vis" [effSort, effSort] boolSort
  soRel <- mkFreshFuncDecl "so" [effSort, effSort] boolSort
  sameobjRel <- mkFreshFuncDecl "sameobj" [effSort, effSort] boolSort
  sameTxnRel <- mkFreshFuncDecl "sameTxn" [effSort, effSort] boolSort
  curTxnRel <- mkFreshFuncDecl "curTxn" [effSort] boolSort
  operRel <- mkFreshFuncDecl "oper" [effSort] operSort

  return $ Z3CtrtState effSort operSort visRel soRel sameobjRel sameTxnRel curTxnRel operRel M.empty [] M.empty

res2Bool :: Result -> Bool
res2Bool Unsat = True
res2Bool Sat = False

not_ :: Z3Ctrt -> Z3Ctrt
not_ (Z3Ctrt m) = Z3Ctrt $ do
  ast <- m
  lift $ mkNot ast

typecheck :: Z3 Sort -> StateT Z3CtrtState Z3 Bool -> IO Bool
typecheck mkOperSort core = evalZ3 $ do
  operSort <- mkOperSort
  st <- mkZ3CtrtState operSort
  (res, _) <- runStateT core st
  return res

isWellTyped :: OperationClass a => Contract a -> Z3 Sort -> IO Bool
isWellTyped c mkOperSort = typecheck mkOperSort $ do
  (curEff, _) <- newEff

  let an1 = fol2Z3Ctrt $ sc curEff
  let cn1 = fol2Z3Ctrt $ c curEff
  let test1 = prop2Z3Ctrt $ mkRawImpl an1 cn1
  assertProp "SC_IMPL_CTRT" $ not_ test1
  lift $ res2Bool <$> check

hbo :: OperationClass a => Effect -> Effect -> Prop a
hbo = AppRel $ TC $ ((So ∩ SameObj) ∪ Vis)

sc :: Contract ()
sc x = forall_ $ \a ->  liftProp $ (hbo a x ∨ hbo x a ∨ AppRel SameEff a x) ∧
                                   (hbo a x ⇒ vis a x) ∧
                                   (hbo x a ⇒ vis x a)

cc :: Contract ()
cc x = forall_ $ \a -> liftProp $ hbo a x ⇒ vis a x

cv :: Contract ()
cv x = forall2_ $ \a b -> liftProp $ (hbo a b ∧ vis b x) ⇒ vis a x

rc :: Fol ()
rc = forall3_ $ \a b c -> liftProp $ trans[[a,b],[c]] ∧ sameObjList [a,b,c] ∧ vis a c ⇒ vis b c

mav :: Fol ()
mav = forall4_ $ \a b c d -> forall3_ $ \e f g -> liftProp $
        (trans[[a,b],[c,d]] ∧ sameObj b d ∧ vis c a ∧ AppRel (So ∪ SameEff) a b ⇒ vis d b) ∧
        (trans[[e,f],[g]] ∧ sameObjList [e,f,g] ∧ vis e g ⇒ vis f g)

rr :: Fol ()
rr = forall4_ $ \a b c d -> forall3_ $ \e f g -> liftProp $
        (trans[[a,b],[c,d]] ∧ sameObj b d ∧ vis c a ⇒ vis d b) ∧
        (trans[[e,f],[g]] ∧ sameObjList [e,f,g] ∧ vis e g ⇒ vis f g)

mkRawImpl :: Z3Ctrt -> Z3Ctrt -> Prop ()
mkRawImpl a b = (Raw a) ⇒ (Raw b)

mkRawImpl2 :: (OperationClass a, OperationClass b) => Fol a -> Fol b -> Z3Ctrt
mkRawImpl2 a b =
  let p::Prop () = (Raw $ fol2Z3Ctrt a) ⇒ (Raw $ fol2Z3Ctrt b)
  in prop2Z3Ctrt p

isUnavailable :: OperationClass a  => Contract a -> Z3 Sort -> IO Bool
isUnavailable c mkOperSort =
  typecheck mkOperSort $ do
    assertBasicAxioms
    (curEff, _) <- newEff

    let an1 = fol2Z3Ctrt $ sc curEff
    let cn1 = fol2Z3Ctrt $ c curEff
    let test1 = prop2Z3Ctrt $ mkRawImpl an1 cn1
    assertProp "SC_IMPL_CTRT" $ not_ test1

    let an2 = fol2Z3Ctrt $ c curEff
    let cn2 = fol2Z3Ctrt $ cc curEff
    let test2 = prop2Z3Ctrt $ Not $ mkRawImpl an2 cn2
    assertProp "CTRT_NOT_IMPL_CC" $ not_ test2
    lift $ res2Bool <$> check

isStickyAvailable :: OperationClass a  => Contract a -> Z3 Sort -> IO Bool
isStickyAvailable c mkOperSort =
  typecheck mkOperSort $ do
    assertBasicAxioms
    (curEff, _) <- newEff

    let an1 = fol2Z3Ctrt $ cc curEff
    let cn1 = fol2Z3Ctrt $ c curEff
    let test1 = prop2Z3Ctrt $ mkRawImpl an1 cn1
    assertProp "CC_IMPL_CTRT" $ not_ test1

    let an2 = fol2Z3Ctrt $ c curEff
    let cn2 = fol2Z3Ctrt $ cv curEff
    let test2 = prop2Z3Ctrt $ Not $ mkRawImpl an2 cn2
    assertProp "CTRT_NOT_IMPL_CV" $ not_ test2
    lift $ res2Bool <$> check

isHighlyAvailable :: OperationClass a  => Contract a -> Z3 Sort -> IO Bool
isHighlyAvailable c mkOperSort =
  typecheck mkOperSort $ do
    assertBasicAxioms
    (curEff, _) <- newEff

    let an1 = fol2Z3Ctrt $ cv curEff
    let cn1 = fol2Z3Ctrt $ c curEff
    let test1 = prop2Z3Ctrt $ mkRawImpl an1 cn1
    assertProp "CV_IMPL_CTRT" $ not_ test1
    lift $ res2Bool <$> check

underReadCommitted :: OperationClass a => Fol a -> Z3 Sort -> IO Bool
underReadCommitted c mkOperSort =
  typecheck mkOperSort $ do
    assertBasicAxioms
    assertProp "RC_IMPL_CTRT" $ not_ (mkRawImpl2 rc c)
    lift $ res2Bool <$> check

underMonotonicAtomicView :: OperationClass a => Fol a -> Z3 Sort -> IO Bool
underMonotonicAtomicView c mkOperSort = do
    let test1 :: Fol () = liftProp . Raw $ mkRawImpl2 mav c
    let test2 :: Fol () = liftProp . Raw $ mkRawImpl2 c rc
    r1 <- isValidProto mkOperSort "MAV_IMPL_CTRT" test1
    if r1
    then isValidProto mkOperSort "CTRT_IMPL_RC" test2
    else return False

underRepeatableRead :: OperationClass a => Fol a -> Z3 Sort -> IO Bool
underRepeatableRead c mkOperSort = do
    let test1 :: Fol () = liftProp . Raw $ mkRawImpl2 rr c
    let test2 :: Fol () = liftProp . Raw $ mkRawImpl2 c mav
    r1 <- isValidProto mkOperSort "RR_IMPL_CTRT" test1
    if r1
    then isValidProto mkOperSort "CTRT_IMPL_MAV" test2
    else return False

classifyTxnContract :: OperationClass a => Fol a -> String -> Q TxnKind
classifyTxnContract c info = do
  mkOperSort <- mkMkZ3OperSort
  runIO $ do
    res <- underReadCommitted c mkOperSort
    if res then return ReadCommitted
    else do
      res <- underMonotonicAtomicView c mkOperSort
      if res then return MonotonicAtomicView
      else do
        res <- underRepeatableRead c mkOperSort
        if res then return RepeatableRead
        else fail $ info ++ " contract is not well-typed"

classifyOperContract :: OperationClass a => Contract a -> String -> Q Availability
classifyOperContract c info = do
  mkOperSort <- mkMkZ3OperSort
  runIO $ do
    isWt <- isWellTyped c mkOperSort
    if not isWt then fail $ info ++ " contract is not well-typed"
    else do
      res <- isHighlyAvailable c mkOperSort
      if res then return High
      else do
        res <- isStickyAvailable c mkOperSort
        if res then return Sticky
        else do
          res <- isUnavailable c mkOperSort
          if res then return Un
          else fail $ info ++ " -- strange contract"

isValidProto :: OperationClass a => Z3 Sort -> String -> Fol a -> IO Bool
isValidProto mkOperSort str c = typecheck mkOperSort $ do
  assertBasicAxioms
  assertProp str $ not_ $ fol2Z3Ctrt c
  lift $ res2Bool <$> check


isValid :: OperationClass a => String -> Fol a -> IO Bool
isValid = isValidProto dummyZ3Sort

isSat :: OperationClass a => String -> Fol a -> IO Bool
isSat str c = typecheck dummyZ3Sort $ do
  assertBasicAxioms
  assertProp str $ fol2Z3Ctrt c
  r <- lift $ check
  return $ case r of {Unsat -> False; Sat -> True}
