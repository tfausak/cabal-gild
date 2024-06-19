module CabalGild.Unstable.Class.MonadLog where

import qualified CabalGild.Unstable.Type.Severity as Severity
import qualified System.IO as IO

-- | A 'Monad' that can also log messages.
class (Monad m) => MonadLog m where
  -- | Logs the given message followed by a newline.
  logLn :: Severity.Severity -> String -> m ()

-- | Uses 'putStrLn'.
instance MonadLog IO where
  logLn severity = case severity of
    Severity.Info -> IO.putStrLn
    Severity.Warn -> IO.hPutStrLn IO.stderr

info :: (MonadLog m) => String -> m ()
info = logLn Severity.Info

warn :: (MonadLog m) => String -> m ()
warn = logLn Severity.Warn
