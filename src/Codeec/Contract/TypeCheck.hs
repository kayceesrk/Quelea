{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Codeec.Contract.TypeCheck (
) where


import Codeec.Types
import Codeec.Contract.Language
import Z3.Monad hiding (mkFreshFuncDecl, mkFreshConst, assertCnstr, push, pop,
                        check, getModel)
import qualified Z3.Monad as Z3M (mkFreshFuncDecl, mkFreshConst, assertCnstr,
                                  push, pop, check, getModel)
import qualified Data.Map as M
import qualified Data.Set as S

-------------------------------------------------------------------------------
-- Types

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
