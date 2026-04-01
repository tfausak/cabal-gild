module CabalGild.Unstable.Type.VersionRange.Simple where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified CabalGild.Unstable.Type.VersionRange.Operator as Operator
import qualified CabalGild.Unstable.Type.VersionRange.Versions as Versions
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.PrettyPrint as PrettyPrint

data Simple
  = Any
  | None
  | Op Operator.Operator Versions.Versions
  deriving (Eq, Ord, Show)

parse :: (Parsec.CabalParsing m) => m Simple
parse =
  Parse.choice
    [ Parse.try $ Any <$ Parse.token "-any",
      None <$ Parse.token "-none",
      Op <$> Operator.parse <*> Versions.parse
    ]

render :: Simple -> PrettyPrint.Doc
render x =
  case x of
    Any -> PrettyPrint.text "-any"
    None -> PrettyPrint.text "-none"
    Op o vs -> Operator.render o <> Versions.render vs
