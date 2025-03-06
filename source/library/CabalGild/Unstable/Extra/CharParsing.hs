module CabalGild.Unstable.Extra.CharParsing where

import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec

-- | Wraps the given parser in curly braces.
braces :: (Parsec.CabalParsing m) => m a -> m a
braces = Parse.between (token "{") (token "}")

-- | Wraps the given parser in parentheses.
parens :: (Parsec.CabalParsing m) => m a -> m a
parens = Parse.between (token "(") (token ")")

-- | Parses the given string followed by any amount of blank space.
token :: (Parsec.CabalParsing m) => String -> m ()
token s = Parse.string s *> Parse.spaces
