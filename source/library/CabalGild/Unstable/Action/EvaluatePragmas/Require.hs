{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Action.EvaluatePragmas.Require where

import Bluefin.Eff (Eff, (:>))
import qualified Bluefin.Exception as Exception
import qualified CabalGild.Unstable.Exception.UnsatisfiedRequire as UnsatisfiedRequire
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified CabalGild.Unstable.Type.VersionRange as VR
import qualified Control.Exception as E
import qualified Control.Monad as Monad
import qualified Data.Version as Version
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Types.Version as CabalVersion
import qualified Distribution.Types.VersionRange as CabalVR
import qualified Paths_cabal_gild as This

run ::
  (eX :> es) =>
  Exception.Exception E.SomeException eX ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  Eff es ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run ex (fs, cs) = (,) <$> traverse (field ex) fs <*> checkRequire ex cs

field ::
  (eX :> es) =>
  Exception.Exception E.SomeException eX ->
  Fields.Field (p, Comments.Comments q) ->
  Eff es (Fields.Field (p, Comments.Comments q))
field ex f = case f of
  Fields.Field n fls -> do
    requireComments ex . snd $ Name.annotation n
    mapM_ (requireComments ex . snd . FieldLine.annotation) fls
    pure f
  Fields.Section n sas fs -> do
    requireComments ex . snd $ Name.annotation n
    Fields.Section n sas <$> traverse (field ex) fs

requireComments ::
  (eX :> es) =>
  Exception.Exception E.SomeException eX ->
  Comments.Comments q ->
  Eff es ()
requireComments ex cs = do
  Monad.void $ checkRequire ex (Comments.before cs)
  Monad.void $ checkRequire ex (Comments.after cs)

-- | Walks a list of comments. When a require pragma is found, validates that
-- the current cabal-gild version satisfies the specified range.
checkRequire ::
  (eX :> es) =>
  Exception.Exception E.SomeException eX ->
  [Comment.Comment q] ->
  Eff es [Comment.Comment q]
checkRequire _ [] = pure []
checkRequire ex (c : cs) = case Parsec.simpleParsecBS $ Comment.value c of
  Just (Pragma.Pragma (Require vr)) -> do
    let cabalVR = VR.toCabalVersionRange vr
        currentVersion = CabalVersion.mkVersion $ Version.versionBranch This.version
    Monad.unless (CabalVR.withinRange currentVersion cabalVR) $
      Exception.throw ex . E.toException $
        UnsatisfiedRequire.UnsatisfiedRequire
          { UnsatisfiedRequire.actual = This.version,
            UnsatisfiedRequire.range = vr
          }
    (c :) <$> checkRequire ex cs
  Nothing -> (c :) <$> checkRequire ex cs

newtype Require = Require VR.VersionRange
  deriving (Eq, Show)

instance Parsec.Parsec Require where
  parsec = do
    Monad.void $ CharParsing.string "require"
    CharParsing.skipSpaces1
    Require <$> Parsec.parsec
