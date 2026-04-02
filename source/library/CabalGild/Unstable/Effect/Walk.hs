{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Effect.Walk where

import Bluefin.Compound (useImpl)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE)
import qualified Bluefin.IO as IO
import qualified System.FilePattern as FilePattern
import qualified System.FilePattern.Directory as FilePattern

data Walk e = MkWalk
  { walkImpl :: FilePath -> [FilePattern.FilePattern] -> [FilePattern.FilePattern] -> Eff e [FilePath]
  }

walk :: (e :> es) => Walk e -> FilePath -> [FilePattern.FilePattern] -> [FilePattern.FilePattern] -> Eff es [FilePath]
walk (MkWalk f) d i x = useImpl $ f d i x

makeWalkIO :: IOE e -> Walk e
makeWalkIO ioe = MkWalk $ \d i x -> IO.effIO ioe $ FilePattern.getDirectoryFilesIgnore d i x
