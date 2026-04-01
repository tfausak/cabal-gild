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

-- | Parses with @||@ at lower precedence than @&&@, matching Cabal.
parse :: (Parsec.CabalParsing m) => m a -> m (Complex a)
parse p = do
  lhs <- andExpr p
  orRest p lhs

-- | Parses a chain of @&&@-separated atoms.
andExpr :: (Parsec.CabalParsing m) => m a -> m (Complex a)
andExpr p = do
  lhs <- atom p
  andRest p lhs

atom :: (Parsec.CabalParsing m) => m a -> m (Complex a)
atom p =
  Parse.choice
    [ Par <$> Parse.parens (parse p),
      Simple <$> p
    ]

andRest :: (Parsec.CabalParsing m) => m a -> Complex a -> m (Complex a)
andRest p lhs =
  Parse.choice
    [ Parse.try $ do Parse.token "&&"; rhs <- atom p; andRest p (And lhs rhs),
      pure lhs
    ]

orRest :: (Parsec.CabalParsing m) => m a -> Complex a -> m (Complex a)
orRest p lhs =
  Parse.choice
    [ Parse.try $ do Parse.token "||"; rhs <- andExpr p; orRest p (Or lhs rhs),
      pure lhs
    ]

render :: (a -> PrettyPrint.Doc) -> Complex a -> PrettyPrint.Doc
render f x =
  case x of
    Par y -> PrettyPrint.parens $ render f y
    And l r -> PrettyPrint.hsep [render f l, PrettyPrint.text "&&", render f r]
    Or l r -> PrettyPrint.hsep [render f l, PrettyPrint.text "||", render f r]
    Simple y -> f y
