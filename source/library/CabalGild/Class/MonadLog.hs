module CabalGild.Class.MonadLog where

import qualified CabalGild.Type.Severity as Severity
import qualified System.IO as IO

-- | A 'Monad' that can also log messages.
class (Monad m) => MonadLog m where
  -- | Logs the given message followed by a newline at the given severity.
  logLn :: Severity.Severity -> String -> m ()

-- | Uses 'putStrLn'.
instance MonadLog IO where
  logLn sev = case sev of
    Severity.Info -> putStrLn
    Severity.Warn -> IO.hPutStrLn IO.stderr

-- | Shortcut for 'logLn' at 'Severity.Info'.
info :: (MonadLog m) => String -> m ()
info = logLn Severity.Info

-- | Shortcut for 'logLn' at 'Severity.Warn'.
warn :: (MonadLog m) => String -> m ()
warn = logLn Severity.Warn
