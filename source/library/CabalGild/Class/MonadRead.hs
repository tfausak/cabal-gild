module CabalGild.Class.MonadRead where

import qualified CabalGild.Type.Input as Input
import qualified Data.ByteString as ByteString

-- | A 'Monad' that can also read input, either from standard input (STDIN) or
-- from a file.
class (Monad m) => MonadRead m where
  -- | Reads input from the given 'Input.Input'.
  read :: Input.Input -> m ByteString.ByteString

-- | Uses 'ByteString.getContents' or 'ByteString.readFile'.
instance MonadRead IO where
  read i = case i of
    Input.Stdin -> ByteString.getContents
    Input.File f -> ByteString.readFile f
