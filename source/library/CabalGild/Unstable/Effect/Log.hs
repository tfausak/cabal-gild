{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Effect.Log where

import Bluefin.Compound (useImpl)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE)
import qualified Bluefin.IO as IO

data Log e = MkLog
  { logLnImpl :: String -> Eff e ()
  }

logLn :: (e :> es) => Log e -> String -> Eff es ()
logLn (MkLog f) s = useImpl $ f s

makeLogIO :: IOE e -> Log e
makeLogIO ioe = MkLog $ \s -> IO.effIO ioe $ putStrLn s
