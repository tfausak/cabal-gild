module CabalGild.Class.MonadWrite where

import qualified Data.ByteString as ByteString

-- | A 'Monad' that can also write output, either to standard output (STDOUT)
-- or to a file.
class (Monad m) => MonadWrite m where
  -- | Writes output to the given file, or to STDOUT if the given file is
  -- 'Nothing'.
  write :: Maybe FilePath -> ByteString.ByteString -> m ()

-- | Uses 'ByteString.putStr' or 'ByteString.writeFile'.
instance MonadWrite IO where
  write = maybe ByteString.putStr ByteString.writeFile
