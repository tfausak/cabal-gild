module CabalGild.Unstable.Class.MonadWalk where

import qualified System.FilePattern as FilePattern
import qualified System.FilePattern.Directory as FilePattern

-- | A 'Monad' that can also "walk" a directory to discover files within it.
class (Monad m) => MonadWalk m where
  -- | This has the same signature and semantics as
  -- 'FilePattern.getDirectoryFilesIgnore'. The first argument is the directory
  -- to walk. The second argument is a list of patterns to include. The third
  -- argument is a list of patterns to exclude.
  walk :: FilePath -> [FilePattern.FilePattern] -> [FilePattern.FilePattern] -> m [FilePath]

-- | Uses 'FilePattern.getDirectoryFilesIgnore'.
instance MonadWalk IO where
  walk = FilePattern.getDirectoryFilesIgnore
