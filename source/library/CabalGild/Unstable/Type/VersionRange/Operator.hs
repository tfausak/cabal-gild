module CabalGild.Unstable.Type.VersionRange.Operator where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.PrettyPrint as PrettyPrint

data Operator
  = Caret
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  deriving (Eq, Ord, Show)

parse :: (Parsec.CabalParsing m) => m Operator
parse =
  Parse.choice
    [ Caret <$ Parse.token "^>=",
      Parse.try $ Ge <$ Parse.token ">=",
      Gt <$ Parse.token ">",
      Parse.try $ Le <$ Parse.token "<=",
      Lt <$ Parse.token "<",
      Eq <$ Parse.token "=="
    ]

render :: Operator -> PrettyPrint.Doc
render x =
  case x of
    Caret -> PrettyPrint.text "^>="
    Ge -> PrettyPrint.text ">="
    Gt -> PrettyPrint.char '>'
    Le -> PrettyPrint.text "<="
    Lt -> PrettyPrint.char '<'
    Eq -> PrettyPrint.text "=="
