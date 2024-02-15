module CabalGild.Class.MonadRead where

import qualified Data.ByteString as ByteString
import qualified System.File.OsPath as File
import qualified System.OsPath as OsPath

class (Monad m) => MonadRead m where
  read :: Maybe OsPath.OsPath -> m ByteString.ByteString

instance MonadRead IO where
  read = maybe ByteString.getContents File.readFile'
