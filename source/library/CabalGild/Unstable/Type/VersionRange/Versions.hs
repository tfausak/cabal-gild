module CabalGild.Unstable.Type.VersionRange.Versions where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified CabalGild.Unstable.Type.VersionRange.Version as Version
import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.PrettyPrint as PrettyPrint

data Versions
  = One Version.Version
  | Set (Set.Set Version.Version)
  deriving (Eq, Ord, Show)

parse :: (Parsec.CabalParsing m) => m Versions
parse =
  Parse.choice
    [ One <$> Version.parse,
      Set . Set.fromList <$> Parse.braces (Parse.sepBy Version.parse $ Parse.token ",")
    ]

render :: Versions -> PrettyPrint.Doc
render x =
  case x of
    One v -> Version.render v
    Set vs ->
      PrettyPrint.braces
        . PrettyPrint.hsep
        . PrettyPrint.punctuate PrettyPrint.comma
        . fmap Version.render
        $ Set.toAscList vs
