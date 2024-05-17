module CabalGild.Unstable.Action.EvaluatePragmas where

import qualified CabalGild.Unstable.Class.MonadWalk as MonadWalk
import qualified CabalGild.Unstable.Exception.InvalidOption as InvalidOption
import qualified CabalGild.Unstable.Exception.UnknownOption as UnknownOption
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.ModuleName as ModuleName
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.String as String
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Fields as Fields
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Utils.Generic as Utils
import qualified System.Console.GetOpt as GetOpt
import qualified System.FilePath as FilePath
import qualified System.FilePath.Windows as FilePath.Windows

-- | High level wrapper around 'field' that makes this action easier to compose
-- with other actions.
run ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  FilePath ->
  ([Fields.Field (p, [Comment.Comment q])], cs) ->
  m ([Fields.Field (p, [Comment.Comment q])], cs)
run p (fs, cs) = (,) <$> traverse (field p) fs <*> pure cs

-- | Evaluates pragmas within the given field. Or, if the field is a section,
-- evaluates pragmas recursively within the fields of the section.
field ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  FilePath ->
  Fields.Field (p, [Comment.Comment q]) ->
  m (Fields.Field (p, [Comment.Comment q]))
field p f = case f of
  Fields.Field n fls -> fmap (Maybe.fromMaybe f) . MaybeT.runMaybeT $ do
    Monad.guard $ Set.member (Name.value n) relevantFieldNames
    comment <- hoistMaybe . Utils.safeLast . snd $ Name.annotation n
    pragma <- hoistMaybe . Parsec.simpleParsecBS $ Comment.value comment
    case pragma of
      Pragma.Discover ds -> discover p n fls ds
  Fields.Section n sas fs -> Fields.Section n sas <$> traverse (field p) fs

-- | If modules are discovered for a field, that fields lines are completely
-- replaced.
discover ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  FilePath ->
  Fields.Name (p, [c]) ->
  [Fields.FieldLine (p, [c])] ->
  [String] ->
  MaybeT.MaybeT m (Fields.Field (p, [c]))
discover p n fls ds = do
  let (strs, args, opts, errs) =
        GetOpt.getOpt'
          GetOpt.Permute
          [ GetOpt.Option [] ["exclude"] (GetOpt.ReqArg id "FILE") ""
          ]
          ds
  mapM_ (Exception.throwM . UnknownOption.fromString) opts
  mapM_ (Exception.throwM . InvalidOption.fromString) errs
  let root = FilePath.takeDirectory p
      directories =
        FilePath.dropTrailingPathSeparator
          . normalize
          . FilePath.combine root
          <$> if null args then ["."] else args
  files <- Trans.lift . fmap mconcat $ traverse MonadWalk.walk directories
  let comments = concatMap (snd . FieldLine.annotation) fls
      position =
        maybe (fst $ Name.annotation n) (fst . FieldLine.annotation) $
          Maybe.listToMaybe fls
      -- Exclusion must be computed relative to the directory containing the cabal file (see #71)
      excludedFiles = Set.fromList $ fmap (normalize . FilePath.combine root) strs
      fieldLines =
        zipWith ModuleName.toFieldLine ((,) position <$> comments : repeat [])
          . Maybe.mapMaybe (toModuleName directories)
          . Maybe.mapMaybe (stripAnyExtension extensions)
          . filter (`Set.notMember` excludedFiles)
          $ fmap normalize files
      -- This isn't great, but the comments have to go /somewhere/.
      name =
        if null fieldLines
          then Lens.over (Name.annotationLens . Lens._2) (comments <>) n
          else n
  pure $ Fields.Field name fieldLines

normalize :: FilePath -> FilePath
normalize =
  FilePath.normalise
    . FilePath.joinPath
    . FilePath.Windows.splitDirectories

-- | These are the names of the fields that can have this action applied to
-- them.
relevantFieldNames :: Set.Set Fields.FieldName
relevantFieldNames =
  Set.fromList $
    fmap
      String.toUtf8
      [ "exposed-modules",
        "other-modules",
        "signatures"
      ]

-- | Attempts to strip any of the given extensions from the file path. If any
-- of them succeed, the result is returned. Otherwise 'Nothing' is returned.
stripAnyExtension :: Set.Set String -> FilePath -> Maybe String
stripAnyExtension es p =
  Maybe.listToMaybe
    . Maybe.mapMaybe (`FilePath.stripExtension` p)
    $ Set.toList es

-- | The set of extensions that should be discovered by this pragma. Any file
-- with one of these extensions will be discovered.
--
-- <https://cabal.readthedocs.io/en/3.10/cabal-package.html#modules-and-preprocessors>
extensions :: Set.Set String
extensions =
  Set.fromList
    [ "chs",
      "cpphs",
      "gc",
      "hs",
      "hsc",
      "hsig",
      "lhs",
      "lhsig",
      "ly",
      "x",
      "y"
    ]

-- | Attempts to convert a file path (without an extension) into a module name
-- by making it relative to one of the given directories.
toModuleName :: [FilePath] -> FilePath -> Maybe ModuleName.ModuleName
toModuleName ds f =
  Maybe.listToMaybe $
    Maybe.mapMaybe (ModuleName.fromFilePath . flip FilePath.makeRelative f) ds

-- | This was added in @transformers-0.6.0.0@. See
-- <https://hub.darcs.net/ross/transformers/issue/49>.
hoistMaybe :: (Applicative f) => Maybe a -> MaybeT.MaybeT f a
hoistMaybe = MaybeT.MaybeT . pure
