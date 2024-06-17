{-# LANGUAGE FlexibleContexts #-}

module CabalGild.Unstable.Type.Condition where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as PE
import qualified Text.PrettyPrint as PrettyPrint

data Condition a
  = Par (Condition a)
  | Not (Condition a)
  | And (Condition a) (Condition a)
  | Or (Condition a) (Condition a)
  | Lit Bool
  | Var a
  deriving (Eq, Show)

parseCondition :: Parsec.ParsecParser a -> Parsec.ParsecParser (Condition a)
parseCondition parseVariable = Parsec.PP $ \csv -> do
  let operators :: (P.Stream s m Char) => PE.OperatorTable s u m (Condition b)
      operators =
        [ [PE.Prefix (Not <$ Parse.char '!' <* Parse.spaces)],
          [PE.Infix (And <$ Parse.string "&&" <* Parse.spaces) PE.AssocRight],
          [PE.Infix (Or <$ Parse.string "||" <* Parse.spaces) PE.AssocRight]
        ]
  Parse.spaces
  PE.buildExpressionParser operators $
    Parsec.unPP
      ( Parse.choice
          [ Par <$> Parse.parens (parseCondition parseVariable),
            Not <$> (Parse.token "!" *> parseCondition parseVariable),
            Lit <$> Parse.try parseLit,
            Var <$> parseVariable
          ]
      )
      csv

parseLit :: (Parsec.CabalParsing m) => m Bool
parseLit =
  Parse.choice
    [ True <$ Parse.token "True",
      True <$ Parse.token "true",
      False <$ Parse.token "False",
      False <$ Parse.token "false"
    ]

prettyCondition :: (a -> PrettyPrint.Doc) -> Condition a -> PrettyPrint.Doc
prettyCondition f x =
  case x of
    Par y -> PrettyPrint.parens (prettyCondition f y)
    Not y -> PrettyPrint.char '!' <> prettyCondition f y
    And y z ->
      PrettyPrint.hsep
        [ prettyCondition f y,
          PrettyPrint.text "&&",
          prettyCondition f z
        ]
    Or y z ->
      PrettyPrint.hsep
        [ prettyCondition f y,
          PrettyPrint.text "||",
          prettyCondition f z
        ]
    Lit y -> PrettyPrint.text $ if y then "true" else "false"
    Var y -> f y
