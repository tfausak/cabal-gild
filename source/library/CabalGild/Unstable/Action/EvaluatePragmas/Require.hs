module CabalGild.Unstable.Action.EvaluatePragmas.Require where

import qualified CabalGild.Unstable.Exception.UnsatisfiedRequire as UnsatisfiedRequire
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified CabalGild.Unstable.Type.VersionRange as VR
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Version as Version
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Types.Version as CabalVersion
import qualified Distribution.Types.VersionRange as CabalVR
import qualified Paths_cabal_gild as This

run ::
  (Exception.MonadThrow m) =>
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  m ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run (fs, cs) = (,) <$> traverse field fs <*> checkRequire cs

field ::
  (Exception.MonadThrow m) =>
  Fields.Field (p, Comments.Comments q) ->
  m (Fields.Field (p, Comments.Comments q))
field f = case f of
  Fields.Field n fls -> do
    requireComments . snd $ Name.annotation n
    mapM_ (requireComments . snd . FieldLine.annotation) fls
    pure f
  Fields.Section n sas fs -> do
    requireComments . snd $ Name.annotation n
    Fields.Section n sas <$> traverse field fs

requireComments ::
  (Exception.MonadThrow m) =>
  Comments.Comments q ->
  m ()
requireComments cs = do
  Monad.void $ checkRequire (Comments.before cs)
  Monad.void $ checkRequire (Comments.after cs)

-- | Walks a list of comments. When a require pragma is found, validates that
-- the current cabal-gild version satisfies the specified range.
checkRequire ::
  (Exception.MonadThrow m) =>
  [Comment.Comment q] ->
  m [Comment.Comment q]
checkRequire [] = pure []
checkRequire (c : cs) = case Parsec.simpleParsecBS $ Comment.value c of
  Just (Pragma.Pragma (Require vr)) -> do
    let cabalVR = VR.toCabalVersionRange vr
        currentVersion = CabalVersion.mkVersion $ Version.versionBranch This.version
    Monad.unless (CabalVR.withinRange currentVersion cabalVR) $
      Exception.throwM
        UnsatisfiedRequire.UnsatisfiedRequire
          { UnsatisfiedRequire.actual = This.version,
            UnsatisfiedRequire.range = vr
          }
    (c :) <$> checkRequire cs
  Nothing -> (c :) <$> checkRequire cs

newtype Require = Require VR.VersionRange
  deriving (Eq, Show)

instance Parsec.Parsec Require where
  parsec = do
    Monad.void $ CharParsing.string "require"
    CharParsing.skipSpaces1
    Require <$> Parsec.parsec
