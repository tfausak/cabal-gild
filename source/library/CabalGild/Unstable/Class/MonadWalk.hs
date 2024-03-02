module CabalGild.Unstable.Class.MonadWalk where

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

-- | A 'Monad' that can also walk the file system.
class (Monad m) => MonadWalk m where
  -- | Lists all files in the given directory and its subdirectories
  -- recursively.
  walk :: FilePath -> m [FilePath]

-- | Uses 'listDirectoryRecursively'.
instance MonadWalk IO where
  walk = listDirectoryRecursively

-- | Lists all files in the given directory and its subdirectories recursively.
-- The order is not guaranteed and may change between different calls. It's
-- also not specified if the results are breadth-first or depth-first.
listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively d = do
  es <- Directory.listDirectory d
  let f e = do
        let p = FilePath.combine d e
        b <- Directory.doesDirectoryExist p
        if b
          then listDirectoryRecursively p
          else pure [p]
  mconcat <$> traverse f es
