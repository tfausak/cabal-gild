module CabalGild.Unstable.Type.Condition where

import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec

data Condition a
  = Par (Condition a)
  | Not (Condition a)
  | And (Condition a) (Condition a)
  | Or (Condition a) (Condition a)
  | Lit Bool
  | Var a
  deriving (Eq, Show)

instance (Parsec.Parsec a) => Parsec.Parsec (Condition a) where
  parsec = parseCondition Parsec.parsec

parseCondition :: (Parsec.CabalParsing m) => m a -> m (Condition a)
parseCondition parseVar =
  Parse.choice
    [ Par <$> Parse.between (token "(") (token ")") (parseCondition parseVar),
      Not <$> parseNot parseVar,
      uncurry And <$> parseAnd parseVar,
      uncurry Or <$> parseOr parseVar,
      Lit <$> parseLit,
      Var <$> parseVar
    ]

parseNot :: (Parsec.CabalParsing m) => m a -> m (Condition a)
parseNot parseVar = token "!" *> parseCondition parseVar

parseAnd :: (Parsec.CabalParsing m) => m a -> m (Condition a, Condition a)
parseAnd parseVar =
  (,)
    <$> parseCondition parseVar
    <* token "&&"
    <*> parseCondition parseVar

parseOr :: (Parsec.CabalParsing m) => m a -> m (Condition a, Condition a)
parseOr parseVar =
  (,)
    <$> parseCondition parseVar
    <* token "||"
    <*> parseCondition parseVar

parseLit :: (Parsec.CabalParsing m) => m Bool
parseLit =
  Parse.choice
    [ True <$ token "True",
      True <$ token "true",
      False <$ token "False",
      False <$ token "false"
    ]

token :: (Parsec.CabalParsing m) => String -> m ()
token s = Parse.string s *> Parse.spaces
