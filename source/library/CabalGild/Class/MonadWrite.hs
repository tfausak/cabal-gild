module CabalGild.Class.MonadWrite where

import qualified Data.ByteString as ByteString
import qualified System.File.OsPath as File
import qualified System.OsPath as OsPath

class (Monad m) => MonadWrite m where
  write :: Maybe OsPath.OsPath -> ByteString.ByteString -> m ()

instance MonadWrite IO where
  write = maybe ByteString.putStr File.writeFile'
