module CabalGild.Unstable.Class.MonadWarn where

import qualified System.IO as IO

-- | A 'Monad' that can also emit warnings.
class (Monad m) => MonadWarn m where
  -- | Emits the given warning message followed by a newline.
  warnLn :: String -> m ()

-- | Uses 'hPutStrLn' on 'stderr'.
instance MonadWarn IO where
  warnLn = IO.hPutStrLn IO.stderr
