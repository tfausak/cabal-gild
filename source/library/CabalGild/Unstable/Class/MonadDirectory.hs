module CabalGild.Unstable.Class.MonadDirectory where

import qualified System.Directory as Dir

class MonadDirectory m where
  listDirectory :: FilePath -> m [FilePath]

instance MonadDirectory IO where
  listDirectory = Dir.listDirectory
