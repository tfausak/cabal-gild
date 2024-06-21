module CabalGild.Unstable.Class.MonadLog where

import qualified CabalGild.Unstable.Type.Severity as Severity
import qualified System.IO as IO

-- | A 'Monad' that can also log messages of a given 'Severity.Severity'.
class (Monad m) => MonadLog m where
  -- | Logs the given message at the given 'Severity.Severity' followed by a
  -- newline.
  logLn :: Severity.Severity -> String -> m ()

-- | Prints to STDOUT for 'Severity.Info' and STDERR for 'Severity.Warn'.
instance MonadLog IO where
  logLn severity = case severity of
    Severity.Info -> IO.putStrLn
    Severity.Warn -> IO.hPutStrLn IO.stderr

-- | Logs the given message at 'Severity.Info'.
info :: (MonadLog m) => String -> m ()
info = logLn Severity.Info

-- | Logs the given message at 'Severity.Warn'.
warn :: (MonadLog m) => String -> m ()
warn = logLn Severity.Warn
