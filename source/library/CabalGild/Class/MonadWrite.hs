module CabalGild.Class.MonadWrite where

import qualified Data.ByteString as ByteString

class (Monad m) => MonadWrite m where
  write :: Maybe FilePath -> ByteString.ByteString -> m ()

instance MonadWrite IO where
  write = maybe ByteString.putStr ByteString.writeFile
