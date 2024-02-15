module CabalGild.Extra.ModuleName where

import qualified CabalGild.Extra.String as String
import qualified Data.List as List
import qualified Distribution.Fields as Fields
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified System.FilePath as FilePath

fromFilePath :: FilePath -> Maybe ModuleName.ModuleName
fromFilePath = Parsec.simpleParsec . List.intercalate "." . FilePath.splitDirectories

toFieldLine :: a -> ModuleName.ModuleName -> Fields.FieldLine a
toFieldLine a = Fields.FieldLine a . String.toUtf8 . Pretty.prettyShow
