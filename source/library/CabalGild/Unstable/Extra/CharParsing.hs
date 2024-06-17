module CabalGild.Unstable.Extra.CharParsing where

import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec

parens :: (Parsec.CabalParsing m) => m a -> m a
parens = Parse.between (token "(") (token ")")

token :: (Parsec.CabalParsing m) => String -> m ()
token s = Parse.string s *> Parse.spaces
