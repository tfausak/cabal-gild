module CabalGild.Class.MonadWalk where

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

class (Monad m) => MonadWalk m where
  walk :: FilePath -> m [FilePath]

instance MonadWalk IO where
  walk = listDirectoryRecursively

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively d = do
  es <- Directory.listDirectory d
  let f e = do
        let p = FilePath.combine d e
        b <- Directory.doesDirectoryExist p
        if b
          then listDirectoryRecursively p
          else pure [p]
  concat <$> mapM f es
