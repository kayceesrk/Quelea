#ifdef MONADIC
import System.ZMQ4.Monadic
#else
import System.ZMQ4
#endif

main = core

#ifdef MONADIC
core = runZMQ $ do
  r <- socket Router
  bind r "inproc://A"
  d <- socket Dealer
  bind d "inproc://B"
  proxy r d Nothing
#else
core = do
  ctxt <- context
  r <- socket ctxt Router
  bind r "inproc://A"
  d <- socket ctxt Dealer
  bind d "inproc://B"
  proxy r d Nothing
#endif
