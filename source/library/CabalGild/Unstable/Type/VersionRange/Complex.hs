module CabalGild.Unstable.Type.VersionRange.Complex where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.PrettyPrint as PrettyPrint

data Complex a
  = Par (Complex a)
  | And a (Complex a)
  | Or a (Complex a)
  | Simple a
  deriving (Eq, Ord, Show)

parse :: (Parsec.CabalParsing m) => m a -> m (Complex a)
parse p =
  Parse.choice
    [ Par <$> Parse.parens (parse p),
      Parse.try $ And <$> p <* Parse.token "&&" <*> parse p,
      Parse.try $ Or <$> p <* Parse.token "||" <*> parse p,
      Simple <$> p
    ]

render :: (a -> PrettyPrint.Doc) -> Complex a -> PrettyPrint.Doc
render f x =
  case x of
    Par y -> PrettyPrint.parens $ render f y
    And l r -> PrettyPrint.hsep [f l, PrettyPrint.text "&&", render f r]
    Or l r -> PrettyPrint.hsep [f l, PrettyPrint.text "||", render f r]
    Simple y -> f y
