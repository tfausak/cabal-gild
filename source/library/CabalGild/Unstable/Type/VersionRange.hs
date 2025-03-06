module CabalGild.Unstable.Type.VersionRange where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Numeric.Natural as Natural
import qualified Text.PrettyPrint as Pretty
import qualified Text.Read as Read

data Part
  = Numeric Natural.Natural
  | Wildcard
  deriving (Eq, Ord, Show)

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

renderNumeric :: Natural.Natural -> Pretty.Doc
renderNumeric = Pretty.text . show

parseWildcard :: (Parsec.CabalParsing m) => m ()
parseWildcard = Parse.token "*"

renderWildcard :: Pretty.Doc
renderWildcard = Pretty.char '*'

parsePart :: (Parsec.CabalParsing m) => m Part
parsePart =
  Parse.choice
    [ Numeric <$> parseNumeric,
      Wildcard <$ parseWildcard
    ]

renderPart :: Part -> Pretty.Doc
renderPart x =
  case x of
    Numeric n -> renderNumeric n
    Wildcard -> renderWildcard

newtype Version
  = MkVersion (NonEmpty.NonEmpty Part)
  deriving (Eq, Ord, Show)

parseVersion :: (Parsec.CabalParsing m) => m Version
parseVersion =
  MkVersion
    <$> Parse.sepByNonEmpty parsePart (Parse.char '.')
    <* Parse.spaces

renderVersion :: Version -> Pretty.Doc
renderVersion (MkVersion parts) =
  mconcat
    . Pretty.punctuate (Pretty.char '.')
    . fmap renderPart
    $ NonEmpty.toList parts

data Versions
  = One Version
  | Set [Version]
  deriving (Eq, Ord, Show)

parseVersions :: (Parsec.CabalParsing m) => m Versions
parseVersions =
  Parse.choice
    [ One <$> parseVersion,
      Set <$> Parse.braces (Parse.sepBy parseVersion $ Parse.token ",")
    ]

renderVersions :: Versions -> Pretty.Doc
renderVersions x =
  case x of
    One v -> renderVersion v
    Set vs ->
      Pretty.braces
        . Pretty.hsep
        . Pretty.punctuate Pretty.comma
        $ fmap renderVersion vs

data Operator
  = Caret
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  deriving (Eq, Ord, Show)

parseOperator :: (Parsec.CabalParsing m) => m Operator
parseOperator =
  Parse.choice
    [ Caret <$ Parse.token "^>=",
      Parse.try $ Ge <$ Parse.token ">=",
      Gt <$ Parse.token ">",
      Parse.try $ Le <$ Parse.token "<=",
      Lt <$ Parse.token "<",
      Eq <$ Parse.token "=="
    ]

renderOperator :: Operator -> Pretty.Doc
renderOperator x =
  case x of
    Caret -> Pretty.text "^>="
    Ge -> Pretty.text ">="
    Gt -> Pretty.char '>'
    Le -> Pretty.text "<="
    Lt -> Pretty.char '<'
    Eq -> Pretty.text "=="

data Simple
  = Any
  | None
  | Op Operator Versions
  deriving (Eq, Ord, Show)

parseSimple :: (Parsec.CabalParsing m) => m Simple
parseSimple =
  Parse.choice
    [ Parse.try $ Any <$ Parse.token "-any",
      None <$ Parse.token "-none",
      Op <$> parseOperator <*> parseVersions
    ]

renderSimple :: Simple -> Pretty.Doc
renderSimple x =
  case x of
    Any -> Pretty.text "-any"
    None -> Pretty.text "-none"
    Op o vs -> renderOperator o <> renderVersions vs

data Complex a
  = Par (Complex a)
  | And a (Complex a)
  | Or a (Complex a)
  | Simple a
  deriving (Eq, Ord, Show)

parseComplex :: (Parsec.CabalParsing m) => m a -> m (Complex a)
parseComplex p =
  Parse.choice
    [ Par <$> Parse.parens (parseComplex p),
      Parse.try $ And <$> p <* Parse.token "&&" <*> parseComplex p,
      Parse.try $ Or <$> p <* Parse.token "||" <*> parseComplex p,
      Simple <$> p
    ]

renderComplex :: (a -> Pretty.Doc) -> Complex a -> Pretty.Doc
renderComplex f x =
  case x of
    Par y -> Pretty.parens $ renderComplex f y
    And l r -> Pretty.hsep [f l, Pretty.text "&&", renderComplex f r]
    Or l r -> Pretty.hsep [f l, Pretty.text "||", renderComplex f r]
    Simple y -> f y

type VersionRange = Complex Simple

parseVersionRange :: (Parsec.CabalParsing m) => m VersionRange
parseVersionRange = parseComplex parseSimple

renderVersionRange :: VersionRange -> Pretty.Doc
renderVersionRange = renderComplex renderSimple
