module CabalGild.Unstable.Type.VersionRange where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
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
    [ 0 <$ Parse.token "0",
      do
        c <- Parse.satisfy $ \c -> '1' <= c && c <= '9'
        cs <- Parse.many Parse.digit
        Parse.spaces
        let s = c : cs
        case Read.readMaybe s of
          Nothing -> fail $ "invalid Natural: " <> show s
          Just n -> pure n
    ]

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
  | Set [Version]
  deriving (Eq, Ord, Show)

parseVersions :: (Parsec.CabalParsing m) => m Versions
parseVersions =
  Parse.choice
    [ One <$> parseVersion,
      Set
        <$> Parse.between
          (Parse.token "{")
          (Parse.token "}")
          (Parse.sepBy parseVersion (Parse.token ","))
    ]

renderVersions :: Versions -> PrettyPrint.Doc
renderVersions x =
  case x of
    One v -> renderVersion v
    Set vs ->
      PrettyPrint.braces
        . PrettyPrint.hsep
        . PrettyPrint.punctuate PrettyPrint.comma
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

renderOperator :: Operator -> PrettyPrint.Doc
renderOperator x =
  case x of
    Caret -> PrettyPrint.text "^>="
    Ge -> PrettyPrint.text ">="
    Gt -> PrettyPrint.char '>'
    Le -> PrettyPrint.text "<="
    Lt -> PrettyPrint.char '<'
    Eq -> PrettyPrint.text "=="

data SimpleConstraint
  = Any
  | None
  | Op Operator Versions
  deriving (Eq, Ord, Show)

parseSimpleConstraint :: (Parsec.CabalParsing m) => m SimpleConstraint
parseSimpleConstraint =
  Parse.choice
    [ Parse.try $ Any <$ Parse.token "-any",
      None <$ Parse.token "-none",
      Op <$> parseOperator <*> parseVersions
    ]

renderSimpleConstraint :: SimpleConstraint -> PrettyPrint.Doc
renderSimpleConstraint x =
  case x of
    Any -> PrettyPrint.text "-any"
    None -> PrettyPrint.text "-none"
    Op o vs -> renderOperator o <> renderVersions vs

data Constraint a
  = And a (Constraint a)
  | Or a (Constraint a)
  | Par a
  | Simple a
  deriving (Eq, Ord, Show)

parseConstraint :: (Parsec.CabalParsing m) => m a -> m (Constraint a)
parseConstraint p =
  Parse.choice
    [ Parse.try $ And <$> p <* Parse.token "&&" <*> parseConstraint p,
      Parse.try $ Or <$> p <* Parse.token "||" <*> parseConstraint p,
      Par <$> Parse.between (Parse.token "(") (Parse.token ")") p,
      Simple <$> p
    ]

renderConstraint :: (a -> PrettyPrint.Doc) -> Constraint a -> PrettyPrint.Doc
renderConstraint f x =
  case x of
    And l r ->
      PrettyPrint.hsep
        [ f l,
          PrettyPrint.text "&&",
          renderConstraint f r
        ]
    Or l r ->
      PrettyPrint.hsep
        [ f l,
          PrettyPrint.text "||",
          renderConstraint f r
        ]
    Par y -> PrettyPrint.parens (f y)
    Simple y -> f y

type VersionRange = Constraint SimpleConstraint

parseVersionRange :: (Parsec.CabalParsing m) => m VersionRange
parseVersionRange = parseConstraint parseSimpleConstraint

renderVersionRange :: VersionRange -> PrettyPrint.Doc
renderVersionRange = renderConstraint renderSimpleConstraint
