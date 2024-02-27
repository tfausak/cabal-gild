module CabalGild.Class.MonadWrite where

import qualified CabalGild.Type.Output as Output
import qualified Data.ByteString as ByteString

-- | A 'Monad' that can also write output, either to standard output (STDOUT)
-- or to a file.
class (Monad m) => MonadWrite m where
  -- | Writes output to the given 'Output.Output'.
  write :: Output.Output -> ByteString.ByteString -> m ()

-- | Uses 'ByteString.putStr' or 'ByteString.writeFile'.
instance MonadWrite IO where
  write o = case o of
    Output.Stdout -> ByteString.putStr
    Output.File f -> ByteString.writeFile f
