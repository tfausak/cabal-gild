module CabalGild.Unstable.Type.VersionRange.Version where

import qualified CabalGild.Unstable.Type.VersionRange.Part as Part
import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.PrettyPrint as PrettyPrint

newtype Version
  = MkVersion (NonEmpty.NonEmpty Part.Part)
  deriving (Eq, Ord, Show)

parseVersion :: (Parsec.CabalParsing m) => m Version
parseVersion =
  MkVersion
    <$> Parse.sepByNonEmpty Part.parsePart (Parse.char '.')
    <* Parse.spaces

renderVersion :: Version -> PrettyPrint.Doc
renderVersion (MkVersion parts) =
  mconcat
    . PrettyPrint.punctuate (PrettyPrint.char '.')
    . fmap Part.renderPart
    $ NonEmpty.toList parts
