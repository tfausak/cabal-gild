module CabalGild.Unstable.Type.Variable where

import qualified CabalGild.Unstable.Extra.CharParsing as Parse
import qualified CabalGild.Unstable.Type.VersionRange as VersionRange
import qualified Data.Char as Char
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Compiler as Compiler
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.System as System
import qualified Distribution.Types.Flag as Flag
import qualified Text.PrettyPrint as PrettyPrint

-- | Similar to 'Distribution.Types.ConfVar.ConfVar', but with different
-- parsing and pretty-printing behavior.
data Variable
  = Arch System.Arch
  | Flag Flag.FlagName
  | Impl Compiler.CompilerFlavor (Maybe VersionRange.VersionRange)
  | Os System.OS
  deriving (Eq, Show)

-- | Parses a 'Variable'. This is generally as permissive as possible.
parseVariable :: (Parsec.CabalParsing m) => m Variable
parseVariable =
  Parse.choice
    [ Arch <$> parseArch,
      Flag <$> parseFlag,
      uncurry Impl <$> parseImpl,
      Os <$> parseOs
    ]

-- | Parses an 'Arch'.
parseArch :: (Parsec.CabalParsing m) => m System.Arch
parseArch =
  fmap (System.classifyArch System.Permissive) $
    Parse.token "arch" *> Parse.parens parseIdent

-- | Parses a 'Flag'.
parseFlag :: (Parsec.CabalParsing m) => m Flag.FlagName
parseFlag = Parse.token "flag" *> Parse.parens (Parsec.parsec <* Parse.spaces)

-- | Parses an 'Impl'.
parseImpl :: (Parsec.CabalParsing m) => m (Compiler.CompilerFlavor, Maybe VersionRange.VersionRange)
parseImpl = do
  Parse.token "impl"
  Parse.parens $
    (,)
      <$> Parsec.parsec
      <* Parse.spaces
      <*> Parse.optional VersionRange.parseVersionRange
      <* Parse.spaces

-- | Parses an 'Os'.
parseOs :: (Parsec.CabalParsing m) => m System.OS
parseOs =
  fmap (System.classifyOS System.Permissive) $
    Parse.token "os" *> Parse.parens parseIdent

-- | Parses an identifier. This is more permissive than anything provided by
-- Cabal. Any run of alphanumeric characters, underscores, or hyphens is
-- considered an identifier.
parseIdent :: (Parsec.CabalParsing m) => m String
parseIdent =
  let isIdent c = Char.isAlphaNum c || c == '_' || c == '-'
   in Parse.munch1 isIdent <* Parse.spaces

-- | Pretty-prints a 'Variable'.
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
        <> PrettyPrint.parens (Pretty.pretty y PrettyPrint.<+> foldMap VersionRange.renderVersionRange z)
    Os y ->
      PrettyPrint.text "os"
        <> PrettyPrint.parens (Pretty.pretty y)
