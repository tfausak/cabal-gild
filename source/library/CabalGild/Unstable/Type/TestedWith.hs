module CabalGild.Unstable.Type.TestedWith where

import qualified CabalGild.Unstable.Type.VersionRange as VersionRange
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Compiler as Compiler
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Text.PrettyPrint as PrettyPrint

data TestedWith = MkTestedWith
  { compilerFlavor :: Compiler.CompilerFlavor,
    versionRange :: Maybe VersionRange.VersionRange
  }
  deriving (Eq, Ord, Show)

instance Parsec.Parsec TestedWith where
  parsec =
    MkTestedWith
      <$> Parsec.parsec
      <* Parse.spaces
      <*> Parse.optional VersionRange.parse

instance Pretty.Pretty TestedWith where
  pretty x =
    PrettyPrint.hsep
      [ Pretty.pretty $ compilerFlavor x,
        foldMap VersionRange.pretty $ versionRange x
      ]
