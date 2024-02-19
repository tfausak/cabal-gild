module CabalGild.Action.EvaluatePragmas where

import qualified CabalGild.Class.MonadWalk as MonadWalk
import qualified CabalGild.Extra.ModuleName as ModuleName
import qualified CabalGild.Extra.Name as Name
import qualified CabalGild.Extra.String as String
import qualified CabalGild.Type.Comment as Comment
import qualified CabalGild.Type.Pragma as Pragma
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Utils.Generic as Utils
import qualified System.FilePath as FilePath

-- | High level wrapper around 'fields' that makes this action easier to
-- compose with other actions.
run ::
  (MonadWalk.MonadWalk m) =>
  FilePath ->
  ([Fields.Field [Comment.Comment a]], cs) ->
  m ([Fields.Field [Comment.Comment a]], cs)
run p (fs, cs) = (,) <$> fields p fs <*> pure cs

-- | Evaluates pragmas modules within the given fields.
fields ::
  (MonadWalk.MonadWalk m) =>
  FilePath ->
  [Fields.Field [Comment.Comment a]] ->
  m [Fields.Field [Comment.Comment a]]
fields = mapM . field

-- | Evaluates pragmas within the given field. Or, if the field is a section,
-- evaluates pragmas recursively within the fields of the section.
--
-- If modules are discovered for a field, that fields lines are completely
-- replaced. If anything goes wrong while discovering modules, the original
-- field is returned.
field ::
  (MonadWalk.MonadWalk m) =>
  FilePath ->
  Fields.Field [Comment.Comment a] ->
  m (Fields.Field [Comment.Comment a])
field p f = case f of
  Fields.Field n _ -> fmap (Maybe.fromMaybe f) . MaybeT.runMaybeT $ do
    es <- hoistMaybe . Map.lookup (Name.value n) $ extensions
    c <- hoistMaybe . Utils.safeLast $ Name.annotation n
    x <- hoistMaybe . Parsec.simpleParsecBS $ Comment.value c
    y <- case x of
      Pragma.Discover y -> pure y
    let d = FilePath.combine (FilePath.takeDirectory p) y
    fs <- Trans.lift $ MonadWalk.walk d
    pure
      . Fields.Field n
      . fmap (ModuleName.toFieldLine [])
      . Maybe.mapMaybe (ModuleName.fromFilePath . FilePath.makeRelative d)
      $ Maybe.mapMaybe (stripAnyExtension es) fs
  Fields.Section n sas fs -> Fields.Section n sas <$> fields p fs

-- | Attempts to strip any of the given extensions from the file path. If any
-- of them succeed, the result is returned. Otherwise 'Nothing' is returned.
stripAnyExtension :: Set.Set String -> FilePath -> Maybe String
stripAnyExtension es p =
  Maybe.listToMaybe
    . Maybe.mapMaybe (flip FilePath.stripExtension p)
    $ Set.toList es

-- | A map from field names to the set of extensions that should be discovered
-- for that field.
extensions :: Map.Map Fields.FieldName (Set.Set String)
extensions =
  let (=:) :: String -> [String] -> (Fields.FieldName, Set.Set String)
      k =: v = (String.toUtf8 k, Set.fromList v)
   in Map.fromList
        [ "exposed-modules" =: ["hs"],
          "other-modules" =: ["hs"]
        ]

-- | This was added in @transformers-0.6.0.0@. See
-- <https://hub.darcs.net/ross/transformers/issue/49>.
hoistMaybe :: (Applicative f) => Maybe a -> MaybeT.MaybeT f a
hoistMaybe = MaybeT.MaybeT . pure
