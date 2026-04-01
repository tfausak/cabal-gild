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

parseVersions :: (Parsec.CabalParsing m) => m Versions
parseVersions =
  Parse.choice
    [ One <$> Version.parseVersion,
      Set . Set.fromList <$> Parse.braces (Parse.sepBy Version.parseVersion $ Parse.token ",")
    ]

renderVersions :: Versions -> PrettyPrint.Doc
renderVersions x =
  case x of
    One v -> Version.renderVersion v
    Set vs ->
      PrettyPrint.braces
        . PrettyPrint.hsep
        . PrettyPrint.punctuate PrettyPrint.comma
        . fmap Version.renderVersion
        $ Set.toAscList vs
