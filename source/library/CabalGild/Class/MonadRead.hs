module CabalGild.Class.MonadRead where

import qualified Data.ByteString as ByteString

class (Monad m) => MonadRead m where
  read :: Maybe FilePath -> m ByteString.ByteString

instance MonadRead IO where
  read = maybe ByteString.getContents ByteString.readFile
