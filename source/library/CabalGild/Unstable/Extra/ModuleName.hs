module CabalGild.Unstable.Extra.ModuleName where

import qualified CabalGild.Unstable.Extra.String as String
import qualified Data.List as List
import qualified Distribution.Fields as Fields
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified System.FilePath as FilePath

-- | Parses a 'FilePath' as a 'ModuleName.ModuleName'.
fromFilePath :: FilePath -> Maybe ModuleName.ModuleName
fromFilePath = Parsec.simpleParsec . List.intercalate "." . FilePath.splitDirectories

-- | Converts a 'ModuleName.ModuleName' into a 'Fields.FieldLine'.
toFieldLine :: a -> ModuleName.ModuleName -> Fields.FieldLine a
toFieldLine a = Fields.FieldLine a . String.toUtf8 . Pretty.prettyShow
