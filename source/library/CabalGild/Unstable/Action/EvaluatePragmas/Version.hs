module CabalGild.Unstable.Action.EvaluatePragmas.Version where

import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Data.ByteString as ByteString
import qualified Data.String as String
import qualified Data.Version as Version
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Paths_cabal_gild as This

run ::
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run (fs, cs) = (fmap field fs, expandVersion cs)

field ::
  Fields.Field (p, Comments.Comments q) ->
  Fields.Field (p, Comments.Comments q)
field = fmap $ fmap versionComments

data Version = Version
  deriving (Eq, Show)

instance Parsec.Parsec Version where
  parsec = Version <$ CharParsing.string "version"

versionComments ::
  Comments.Comments q ->
  Comments.Comments q
versionComments cs =
  cs
    { Comments.before = expandVersion (Comments.before cs),
      Comments.after = expandVersion (Comments.after cs)
    }

-- | Walks a list of comments. When a version pragma is found, ensures it is
-- followed by a generated version comment. Any existing generated comment is
-- replaced with the current version.
expandVersion :: [Comment.Comment q] -> [Comment.Comment q]
expandVersion [] = []
expandVersion (c : cs) = case Parsec.simpleParsecBS $ Comment.value c of
  Just (Pragma.Pragma Version) ->
    c : generatedComment c : expandVersion (dropGenerated cs)
  Nothing -> c : expandVersion cs

-- | Creates a generated version comment, borrowing the annotation from the
-- pragma comment.
generatedComment :: Comment.Comment q -> Comment.Comment q
generatedComment pragma =
  Comment.Comment
    { Comment.annotation = Comment.annotation pragma,
      Comment.value = String.fromString generatedText
    }

-- | The full text of a generated version comment (without the @--@ delimiter).
generatedText :: String
generatedText = generatedPrefix <> Version.showVersion This.version

-- | The prefix used to identify generated version comments.
generatedPrefix :: String
generatedPrefix = " Generated with cabal-gild version "

-- | Checks whether a comment is a generated version comment.
isGenerated :: Comment.Comment q -> Bool
isGenerated = (String.fromString generatedPrefix `ByteString.isPrefixOf`) . Comment.value

-- | Drops a leading generated version comment, if present.
dropGenerated :: [Comment.Comment q] -> [Comment.Comment q]
dropGenerated (c : cs) | isGenerated c = cs
dropGenerated cs = cs
