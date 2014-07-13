module Codeec.Contract (
  Rel(..),
  Prop(..),
  Fol,
  Contract,
  Effect,

  true,
  vis,
  so,
  soo,
  sameeff,
  hb,
  (∩),
  (∪),
  (∧),
  (∨),
  (⇒),
  (^+),

  liftProp,
  forall_,
  forallQ_

) where

import Codeec.Contract.Language
import Codeec.Contract.TypeCheck
