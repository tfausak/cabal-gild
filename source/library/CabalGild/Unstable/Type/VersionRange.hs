{-# LANGUAGE FlexibleInstances #-}

module CabalGild.Unstable.Type.VersionRange where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Numeric.Natural as Natural
import qualified Text.PrettyPrint as PrettyPrint
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

renderNumeric :: Natural.Natural -> PrettyPrint.Doc
renderNumeric = PrettyPrint.text . show

parseWildcard :: (Parsec.CabalParsing m) => m ()
parseWildcard = Parse.token "*"

renderWildcard :: PrettyPrint.Doc
renderWildcard = PrettyPrint.char '*'

parsePart :: (Parsec.CabalParsing m) => m Part
parsePart =
  Parse.choice
    [ Numeric <$> parseNumeric,
      Wildcard <$ parseWildcard
    ]

renderPart :: Part -> PrettyPrint.Doc
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

renderVersion :: Version -> PrettyPrint.Doc
renderVersion (MkVersion parts) =
  mconcat
    . PrettyPrint.punctuate (PrettyPrint.char '.')
    . fmap renderPart
    $ NonEmpty.toList parts

data Versions
  = One Version
  | Set (Set.Set Version)
  deriving (Eq, Ord, Show)

parseVersions :: (Parsec.CabalParsing m) => m Versions
parseVersions =
  Parse.choice
    [ One <$> parseVersion,
      Set . Set.fromList <$> Parse.braces (Parse.sepBy parseVersion $ Parse.token ",")
    ]

renderVersions :: Versions -> PrettyPrint.Doc
renderVersions x =
  case x of
    One v -> renderVersion v
    Set vs ->
      PrettyPrint.braces
        . PrettyPrint.hsep
        . PrettyPrint.punctuate PrettyPrint.comma
        . fmap renderVersion
        $ Set.toAscList vs

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

renderOperator :: Operator -> PrettyPrint.Doc
renderOperator x =
  case x of
    Caret -> PrettyPrint.text "^>="
    Ge -> PrettyPrint.text ">="
    Gt -> PrettyPrint.char '>'
    Le -> PrettyPrint.text "<="
    Lt -> PrettyPrint.char '<'
    Eq -> PrettyPrint.text "=="

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

renderSimple :: Simple -> PrettyPrint.Doc
renderSimple x =
  case x of
    Any -> PrettyPrint.text "-any"
    None -> PrettyPrint.text "-none"
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

renderComplex :: (a -> PrettyPrint.Doc) -> Complex a -> PrettyPrint.Doc
renderComplex f x =
  case x of
    Par y -> PrettyPrint.parens $ renderComplex f y
    And l r -> PrettyPrint.hsep [f l, PrettyPrint.text "&&", renderComplex f r]
    Or l r -> PrettyPrint.hsep [f l, PrettyPrint.text "||", renderComplex f r]
    Simple y -> f y

type VersionRange = Complex Simple

instance Parsec.Parsec VersionRange where
  parsec = parseComplex parseSimple

instance Pretty.Pretty VersionRange where
  pretty = renderComplex renderSimple
