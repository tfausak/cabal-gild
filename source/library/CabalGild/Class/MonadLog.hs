module CabalGild.Class.MonadLog where

class (Monad m) => MonadLog m where
  log :: String -> m ()

instance MonadLog IO where
  log = putStr

logLn :: (MonadLog m) => String -> m ()
logLn = CabalGild.Class.MonadLog.log . (<> "\n")
