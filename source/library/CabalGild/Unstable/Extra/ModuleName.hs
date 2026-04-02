{-# OPTIONS_GHC -Wno-deprecations #-}

module CabalGild.Unstable.Extra.ModuleName where

import qualified CabalGild.Unstable.Extra.String as String
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Distribution.Fields as Fields
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Pretty as Pretty
import qualified System.FilePath.Posix as FilePath

-- | Converts a 'FilePath' (without extension) into a 'ModuleName.ModuleName'.
-- The path must use POSIX separators. Returns 'Nothing' if any path component
-- is not a valid module component.
fromFilePath :: FilePath -> Maybe ModuleName.ModuleName
fromFilePath p = do
  let cs = FilePath.splitDirectories p
  Monad.guard $ all ModuleName.validModuleComponent cs
  pure $ ModuleName.fromComponents cs

toCaseFold :: ModuleName.ModuleName -> ModuleName.ModuleName
toCaseFold =
  ModuleName.fromString
    . List.intercalate "."
    . fmap String.toCaseFold
    . ModuleName.components

-- | Converts a 'ModuleName.ModuleName' into a 'Fields.FieldLine'.
toFieldLine :: a -> ModuleName.ModuleName -> Fields.FieldLine a
toFieldLine a = Fields.FieldLine a . String.toUtf8 . Pretty.prettyShow
