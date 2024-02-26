module CabalGild.Class.MonadLog where

-- | A 'Monad' that can also log messages.
class (Monad m) => MonadLog m where
  -- | Logs the given message followed by a newline.
  logLn :: String -> m ()

-- | Uses 'putStrLn'.
instance MonadLog IO where
  logLn = putStrLn

-- | Alias for 'logLn'.
info :: (MonadLog m) => String -> m ()
info = logLn
