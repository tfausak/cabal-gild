module CabalGild.Unstable.Type.VersionRange.Complex where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.PrettyPrint as PrettyPrint

data Complex a
  = Par (Complex a)
  | And (Complex a) (Complex a)
  | Or (Complex a) (Complex a)
  | Simple a
  deriving (Eq, Ord, Show)

parse :: (Parsec.CabalParsing m) => m a -> m (Complex a)
parse p = do
  lhs <- atom p
  rest p lhs

atom :: (Parsec.CabalParsing m) => m a -> m (Complex a)
atom p =
  Parse.choice
    [ Par <$> Parse.parens (parse p),
      Simple <$> p
    ]

rest :: (Parsec.CabalParsing m) => m a -> Complex a -> m (Complex a)
rest p lhs =
  Parse.choice
    [ Parse.try $ do Parse.token "&&"; rhs <- parse p; pure $ And lhs rhs,
      Parse.try $ do Parse.token "||"; rhs <- parse p; pure $ Or lhs rhs,
      pure lhs
    ]

render :: (a -> PrettyPrint.Doc) -> Complex a -> PrettyPrint.Doc
render f x =
  case x of
    Par y -> PrettyPrint.parens $ render f y
    And l r -> PrettyPrint.hsep [render f l, PrettyPrint.text "&&", render f r]
    Or l r -> PrettyPrint.hsep [render f l, PrettyPrint.text "||", render f r]
    Simple y -> f y
