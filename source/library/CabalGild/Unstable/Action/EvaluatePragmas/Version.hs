{-# LANGUAGE CPP #-}

module CabalGild.Unstable.Action.EvaluatePragmas.Version where

import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Data.String as String
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec

field ::
  () =>
  Fields.Field (p, Comments.Comments q) ->
  Fields.Field (p, Comments.Comments q)
field = fmap $ fmap versionComments

data Version = Version
  deriving (Eq, Show)

instance Parsec.Parsec Version where
  parsec = Version <$ CharParsing.string cabalVersionPragma

cabalVersionPragma :: (String.IsString a) => a
cabalVersionPragma = String.fromString "version"

versionComments ::
  () =>
  Comments.Comments q ->
  Comments.Comments q
versionComments cs =
  cs
    { Comments.before = fmap version (Comments.before cs),
      Comments.after = fmap version (Comments.after cs)
    }

version ::
  () =>
  Comment.Comment q ->
  Comment.Comment q
version comment = case Parsec.simpleParsecBS $ Comment.value comment of
  Nothing -> comment
  Just (Pragma.Pragma Version) -> comment {Comment.value = String.fromString versionComment}
  where
    versionComment = " " <> Pragma.cabalGildPragmaPrefix <> " " <> cabalVersionPragma <> " " <> CURRENT_PACKAGE_VERSION
