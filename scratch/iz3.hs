import Z3.Monad
import Control.Applicative
import Control.Monad.IO.Class

main = evalZ3WithInterpolationContext $ do
  is <- mkIntSort
  x <- mkFreshConst "x" is
  y <- mkFreshConst "y" is
  z <- mkFreshConst "z" is
  _2 <- mkInt 2
  --
  x2 <- mkMul [_2, x]
  f1 <- mkEq x2 y
  --
  z2 <- mkMul [_2, z]
  _1 <- mkInt 1
  z2p1 <- mkAdd [z2, _1]
  f2 <- mkEq z2p1 y
  --
  t <- mkTrue
  assertCnstr f1
  assertCnstr f2
  --
  res <- interpolate [f1,f2]
  sres <- mapM astToString res
  liftIO $ mapM_ putStrLn sres
