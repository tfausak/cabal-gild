module CabalGild.Class.MonadLog where

import qualified CabalGild.Type.Severity as Severity

-- | A 'Monad' that can also log messages.
class (Monad m) => MonadLog m where
  -- | Logs the given message followed by a newline at the given severity.
  logLn :: Severity.Severity -> String -> m ()

-- | Uses 'putStrLn'.
instance MonadLog IO where
  logLn = const putStrLn

-- | Shortcut for 'logLn' at 'Severity.Info'.
info :: (MonadLog m) => String -> m ()
info = logLn Severity.Info
