module CabalGild.Unstable.Class.MonadWalk where

import qualified System.FilePattern as FilePattern
import qualified System.FilePattern.Directory as FilePattern

-- | A wrapper around 'FilePattern.getDirectoryFilesIgnore'.
class (Monad m) => MonadWalk m where
  walk :: FilePath -> [FilePattern.FilePattern] -> [FilePattern.FilePattern] -> m [FilePath]

instance MonadWalk IO where
  walk = FilePattern.getDirectoryFilesIgnore
