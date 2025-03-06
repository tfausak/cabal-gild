module CabalGild.Unstable.Type.VersionRange where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Numeric.Natural as Natural
import qualified Text.PrettyPrint as PrettyPrint
import qualified Text.Read as Read

parseToken :: (Parsec.CabalParsing m) => String -> m ()
parseToken s = Parse.string s *> Parse.spaces

data Part
  = Numeric Natural.Natural
  | Wildcard
  deriving (Eq, Ord, Show)

parseNumeric :: (Parsec.CabalParsing m) => m Natural.Natural
parseNumeric =
  Parse.choice
    [ 0 <$ parseToken "0",
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
parseWildcard = parseToken "*"

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
          (parseToken "{")
          (parseToken "}")
          (Parse.sepBy parseVersion (parseToken ","))
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
    [ Caret <$ parseToken "^>=",
      Parse.try $ Ge <$ parseToken ">=",
      Gt <$ parseToken ">",
      Parse.try $ Le <$ parseToken "<=",
      Lt <$ parseToken "<",
      Eq <$ parseToken "=="
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

data Constraint
  = Any
  | None
  | Op Operator Versions
  | Par Constraint
  | And Constraint Constraint
  | Or Constraint Constraint
  deriving (Eq, Ord, Show)

parseConstraint :: (Parsec.CabalParsing m) => m Constraint
parseConstraint =
  Parse.choice
    [ Parse.try $ And <$> parseSimpleConstraint <* parseToken "&&" <*> parseConstraint,
      Parse.try $ Or <$> parseSimpleConstraint <* parseToken "||" <*> parseConstraint,
      parseSimpleConstraint
    ]

parseSimpleConstraint :: (Parsec.CabalParsing m) => m Constraint
parseSimpleConstraint =
  Parse.choice
    [ Parse.try $ Any <$ parseToken "-any",
      None <$ parseToken "-none",
      Op <$> parseOperator <*> parseVersions,
      Par <$> Parse.between (parseToken "(") (parseToken ")") parseConstraint
    ]

renderConstraint :: Constraint -> PrettyPrint.Doc
renderConstraint x =
  case x of
    Any -> PrettyPrint.text "-any"
    None -> PrettyPrint.text "-none"
    Op o vs -> renderOperator o <> renderVersions vs
    Par y -> PrettyPrint.parens (renderConstraint y)
    And l r ->
      PrettyPrint.hsep
        [ renderConstraint l,
          PrettyPrint.text "&&",
          renderConstraint r
        ]
    Or l r ->
      PrettyPrint.hsep
        [ renderConstraint l,
          PrettyPrint.text "||",
          renderConstraint r
        ]
