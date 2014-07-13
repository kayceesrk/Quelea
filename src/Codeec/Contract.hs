module Codeec.Contract (
  Rel(..),
  Prop(..),
  Fol,
  Contract,
  Effect,

  true_,
  vis,
  so,
  soo,
  hb,
  (∩),
  (∪),
  liftProp,
  forall_,
  forallQ_

) where

import Codeec.Contract.Language
import Codeec.Contract.TypeCheck
