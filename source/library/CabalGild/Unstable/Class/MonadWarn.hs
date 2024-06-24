module CabalGild.Unstable.Class.MonadWarn where

import qualified CabalGild.Unstable.Class.Warning as Warning
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified System.IO as IO

-- | A monad that can also emit warnings.
class (Monad m) => MonadWarn m where
  warn :: (Warning.Warning w) => w -> m ()

-- | Prints the warning to STDERR.
instance MonadWarn IO where
  warn = IO.hPutStrLn IO.stderr . mappend "WARNING: " . Warning.displayWarning

-- | Delegates to the inner monad.
instance (MonadWarn m) => MonadWarn (MaybeT.MaybeT m) where
  warn = Trans.lift . warn
