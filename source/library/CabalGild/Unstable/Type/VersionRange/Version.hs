module CabalGild.Unstable.Type.VersionRange.Version where

import qualified CabalGild.Unstable.Type.VersionRange.Part as Part
import qualified Control.Monad as Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.PrettyPrint as PrettyPrint

newtype Version
  = MkVersion (NonEmpty.NonEmpty Part.Part)
  deriving (Eq, Ord, Show)

parse :: (Parsec.CabalParsing m) => m Version
parse = do
  parts <- Parse.sepByNonEmpty Part.parse (Parse.char '.')
  Monad.unless (validWildcards parts) $
    fail "wildcards are only allowed in the final position"
  MkVersion parts <$ Parse.spaces

-- | Wildcards are only valid in the final position of a version.
validWildcards :: NonEmpty.NonEmpty Part.Part -> Bool
validWildcards parts = all isNumeric (NonEmpty.init parts)
  where
    isNumeric (Part.Numeric _) = True
    isNumeric Part.Wildcard = False

render :: Version -> PrettyPrint.Doc
render (MkVersion parts) =
  mconcat
    . PrettyPrint.punctuate (PrettyPrint.char '.')
    . fmap Part.render
    $ NonEmpty.toList parts
