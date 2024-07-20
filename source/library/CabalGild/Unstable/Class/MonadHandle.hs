module CabalGild.Unstable.Class.MonadHandle where

import qualified CabalGild.Unstable.Type.Input as Input
import qualified System.IO as System

class MonadHandle m where
  hIsTerminalDevice :: Input.Input -> m Bool

instance MonadHandle IO where
  hIsTerminalDevice i = case i of
    Input.Stdin -> System.hIsTerminalDevice System.stdin
    _ -> pure False

