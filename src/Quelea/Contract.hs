module Quelea.Contract (
  Rel(..),
  Prop(..),
  EffCol(..),
  Fol,
  Contract,
  Effect,

  true,
  vis,
  so,
  soo,
  sameEff,
  appRel,
  sameObj,
  sameObjList,
  trans,
  hb,
  hbo,
  (∩),
  (∪),
  (∧),
  (∨),
  (⇒),
  (^+),

  liftProp,
  forall_,
  forall2_,
  forall3_,
  forall4_,
  forallQ_,
  forallQ2_,
  forallQ3_,
  forallQ4_,
  isValid,
  isSat,


  -- Only for DEBUG
  fol2Z3Ctrt,
  underMAV,
  dummyZ3Sort
) where

import Quelea.Contract.Language
import Quelea.Contract.TypeCheck
