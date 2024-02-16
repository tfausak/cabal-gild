module CabalGild.Class.MonadLog where

-- | A 'Monad' that can also log messages.
class (Monad m) => MonadLog m where
  -- | Logs the given message. Typical usage should prefer 'logLn'.
  log :: String -> m ()

-- | Uses 'putStr'.
instance MonadLog IO where
  log = putStr

-- | Logs the message followed by a newline.
logLn :: (MonadLog m) => String -> m ()
logLn = CabalGild.Class.MonadLog.log . (<> "\n")
