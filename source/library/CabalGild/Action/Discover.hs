module CabalGild.Action.Discover where

import qualified CabalGild.Class.MonadWalk as MonadWalk
import qualified CabalGild.Extra.ModuleName as ModuleName
import qualified CabalGild.Extra.Name as Name
import qualified CabalGild.Extra.String as String
import qualified CabalGild.Type.Comment as Comment
import qualified CabalGild.Type.Pragma as Pragma
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Utils.Generic as Utils
import qualified System.OsPath as OsPath

run ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  OsPath.OsPath ->
  ([Fields.Field [Comment.Comment a]], cs) ->
  m ([Fields.Field [Comment.Comment a]], cs)
run p (fs, cs) = (,) <$> fields p fs <*> pure cs

fields ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  OsPath.OsPath ->
  [Fields.Field [Comment.Comment a]] ->
  m [Fields.Field [Comment.Comment a]]
fields = mapM . field

field ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  OsPath.OsPath ->
  Fields.Field [Comment.Comment a] ->
  m (Fields.Field [Comment.Comment a])
field p f = case f of
  Fields.Field n _ -> fmap (Maybe.fromMaybe f) . MaybeT.runMaybeT $ do
    Monad.guard $ Set.member (Name.value n) relevantFieldNames
    c <- MaybeT.hoistMaybe . Utils.safeLast $ Name.annotation n
    Pragma.Discover x <- MaybeT.hoistMaybe . Parsec.simpleParsecBS $ Comment.value c
    hs <- OsPath.encodeUtf ".hs"
    let d = OsPath.combine (OsPath.takeDirectory p) x
    fs <- Trans.lift $ MonadWalk.walk d
    pure
      . Fields.Field n
      . fmap (ModuleName.toFieldLine [])
      . Maybe.mapMaybe (ModuleName.fromOsPath . OsPath.makeRelative d)
      $ Maybe.mapMaybe (OsPath.stripExtension hs) fs
  Fields.Section n sas fs -> Fields.Section n sas <$> fields p fs

relevantFieldNames :: Set.Set Fields.FieldName
relevantFieldNames =
  Set.fromList $
    fmap
      String.toUtf8
      [ "exposed-modules",
        "other-modules"
      ]
