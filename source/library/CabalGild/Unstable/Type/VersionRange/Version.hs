module CabalGild.Unstable.Type.VersionRange.Version where

import qualified CabalGild.Unstable.Type.VersionRange.Part as Part
import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.PrettyPrint as PrettyPrint

newtype Version
  = MkVersion (NonEmpty.NonEmpty Part.Part)
  deriving (Eq, Ord, Show)

parse :: (Parsec.CabalParsing m) => m Version
parse =
  MkVersion
    <$> Parse.sepByNonEmpty Part.parse (Parse.char '.')
    <* Parse.spaces

render :: Version -> PrettyPrint.Doc
render (MkVersion parts) =
  mconcat
    . PrettyPrint.punctuate (PrettyPrint.char '.')
    . fmap Part.render
    $ NonEmpty.toList parts
