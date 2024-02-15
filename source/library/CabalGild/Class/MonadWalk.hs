module CabalGild.Class.MonadWalk where

import qualified System.Directory.OsPath as Directory
import qualified System.OsPath as OsPath

class (Monad m) => MonadWalk m where
  walk :: OsPath.OsPath -> m [OsPath.OsPath]

instance MonadWalk IO where
  walk = listDirectoryRecursively

listDirectoryRecursively :: OsPath.OsPath -> IO [OsPath.OsPath]
listDirectoryRecursively d = do
  es <- Directory.listDirectory d
  let f e = do
        let p = OsPath.combine d e
        b <- Directory.doesDirectoryExist p
        if b
          then listDirectoryRecursively p
          else pure [p]
  concat <$> mapM f es
