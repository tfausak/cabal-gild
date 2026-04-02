{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Effect.Warn where

import Bluefin.Compound (useImpl)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE)
import qualified Bluefin.IO as IO
import qualified System.IO as SIO

data Warn e = MkWarn
  { warnLnImpl :: String -> Eff e ()
  }

warnLn :: (e :> es) => Warn e -> String -> Eff es ()
warnLn (MkWarn f) s = useImpl $ f s

makeWarnIO :: IOE e -> Warn e
makeWarnIO ioe = MkWarn $ \s -> IO.effIO ioe $ SIO.hPutStrLn SIO.stderr s
