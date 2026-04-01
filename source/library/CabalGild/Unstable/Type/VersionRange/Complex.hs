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
