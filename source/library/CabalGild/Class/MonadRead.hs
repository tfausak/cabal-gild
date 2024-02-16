module CabalGild.Class.MonadRead where

import qualified Data.ByteString as ByteString

-- | A 'Monad' that can also read input, either from standard input (STDIN) or
-- from a file.
class (Monad m) => MonadRead m where
  -- | Reads input from the given file, or from STDIN if the given file is
  -- 'Nothing'.
  read :: Maybe FilePath -> m ByteString.ByteString

-- | Uses 'ByteString.getContents' or 'ByteString.readFile'.
instance MonadRead IO where
  read = maybe ByteString.getContents ByteString.readFile
