module CabalGild.Unstable.Type.VersionRange.Part where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Numeric.Natural as Natural
import qualified Text.PrettyPrint as PrettyPrint
import qualified Text.Read as Read

data Part
  = Numeric Natural.Natural
  | Wildcard
  deriving (Eq, Ord, Show)

parse :: (Parsec.CabalParsing m) => m Part
parse =
  Parse.choice
    [ Numeric <$> parseNumeric,
      Wildcard <$ parseWildcard
    ]

parseNumeric :: (Parsec.CabalParsing m) => m Natural.Natural
parseNumeric =
  Parse.choice
    [ parseZero,
      parseNonZero
    ]

parseZero :: (Parsec.CabalParsing m) => m Natural.Natural
parseZero = 0 <$ Parse.token "0"

parseNonZero :: (Parsec.CabalParsing m) => m Natural.Natural
parseNonZero = do
  c <- Parse.satisfy $ \c -> '1' <= c && c <= '9'
  cs <- Parse.many Parse.digit
  let s = c : cs
  case Read.readMaybe s of
    Nothing -> fail $ "invalid Natural: " <> show s
    Just n -> n <$ Parse.spaces

parseWildcard :: (Parsec.CabalParsing m) => m ()
parseWildcard = Parse.token "*"

render :: Part -> PrettyPrint.Doc
render x =
  case x of
    Numeric n -> renderNumeric n
    Wildcard -> renderWildcard

renderNumeric :: Natural.Natural -> PrettyPrint.Doc
renderNumeric = PrettyPrint.text . show

renderWildcard :: PrettyPrint.Doc
renderWildcard = PrettyPrint.char '*'
