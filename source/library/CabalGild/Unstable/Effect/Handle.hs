{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Effect.Handle where

import Bluefin.Compound (useImpl)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE)
import qualified Bluefin.IO as IO
import qualified System.IO as SIO

data Handle e = MkHandle
  { isTerminalDeviceImpl :: SIO.Handle -> Eff e Bool
  }

isTerminalDevice :: (e :> es) => Handle e -> SIO.Handle -> Eff es Bool
isTerminalDevice (MkHandle f) h = useImpl $ f h

makeHandleIO :: IOE e -> Handle e
makeHandleIO ioe = MkHandle $ \h -> IO.effIO ioe $ SIO.hIsTerminalDevice h
