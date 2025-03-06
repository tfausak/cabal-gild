module CabalGild.Unstable.Class.MonadHandle where

import qualified System.IO as IO

class MonadHandle m where
  isTerminalDevice :: IO.Handle -> m Bool

instance MonadHandle IO where
  isTerminalDevice = IO.hIsTerminalDevice
