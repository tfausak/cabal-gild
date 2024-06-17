module CabalGild.Unstable.Type.Variable where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified Data.Char as Char
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Compiler as Compiler
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.System as System
import qualified Distribution.Types.Flag as Flag
import qualified Distribution.Types.VersionRange as VersionRange
import qualified Text.PrettyPrint as PrettyPrint

data Variable
  = Arch System.Arch
  | Flag Flag.FlagName
  | Impl Compiler.CompilerFlavor VersionRange.VersionRange
  | Os System.OS
  deriving (Eq, Show)

parseVariable :: (Parsec.CabalParsing m) => m Variable
parseVariable =
  Parse.choice
    [ Arch <$> parseArch,
      Flag <$> parseFlag,
      uncurry Impl <$> parseImpl,
      Os <$> parseOs
    ]

parseArch :: (Parsec.CabalParsing m) => m System.Arch
parseArch =
  fmap (System.classifyArch System.Permissive) $
    Parse.token "arch" *> Parse.parens parseIdent

parseFlag :: (Parsec.CabalParsing m) => m Flag.FlagName
parseFlag = Parse.token "flag" *> Parse.parens (Parsec.parsec <* Parse.spaces)

parseImpl :: (Parsec.CabalParsing m) => m (Compiler.CompilerFlavor, VersionRange.VersionRange)
parseImpl = do
  Parse.token "impl"
  Parse.parens $
    (,)
      <$> Parsec.parsec
      <* Parse.spaces
      <*> Parse.option VersionRange.anyVersion Parsec.parsec
      <* Parse.spaces

parseOs :: (Parsec.CabalParsing m) => m System.OS
parseOs =
  fmap (System.classifyOS System.Permissive) $
    Parse.token "os" *> Parse.parens parseIdent

parseIdent :: (Parsec.CabalParsing m) => m String
parseIdent =
  let isIdent c = Char.isAlphaNum c || c == '_' || c == '-'
   in Parse.munch1 isIdent <* Parse.spaces

prettyVariable :: Variable -> PrettyPrint.Doc
prettyVariable x =
  case x of
    Arch y ->
      PrettyPrint.text "arch"
        <> PrettyPrint.parens (Pretty.pretty y)
    Flag y ->
      PrettyPrint.text "flag"
        <> PrettyPrint.parens (Pretty.pretty y)
    Impl y z ->
      PrettyPrint.text "impl"
        <> PrettyPrint.parens (Pretty.pretty y PrettyPrint.<+> Pretty.pretty z)
    Os y ->
      PrettyPrint.text "os"
        <> PrettyPrint.parens (Pretty.pretty y)
