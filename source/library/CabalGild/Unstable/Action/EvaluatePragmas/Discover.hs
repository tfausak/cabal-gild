{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Action.EvaluatePragmas.Discover where

import Bluefin.Eff (Eff, (:>))
import qualified Bluefin.Exception as Exception
import qualified CabalGild.Unstable.Effect.Walk as Walk
import qualified CabalGild.Unstable.Exception.InvalidOption as InvalidOption
import qualified CabalGild.Unstable.Exception.UnknownOption as UnknownOption
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.FilePath as FilePath
import qualified CabalGild.Unstable.Extra.ModuleName as ModuleName
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.String as String
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.DiscoverTarget as DiscoverTarget
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Control.Exception as E
import qualified Control.Monad as Monad
import qualified Data.Containers.ListUtils as List
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Fields as Fields
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Utils.Generic as Utils
import qualified System.Console.GetOpt as GetOpt
import qualified System.FilePath as FilePath

run ::
  (eX :> es, eW :> es) =>
  Exception.Exception E.SomeException eX ->
  Walk.Walk eW ->
  FilePath ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  Eff es ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run ex walkH p (fs, cs) = (,) <$> traverse (field ex walkH p) fs <*> pure cs

-- | Evaluates pragmas within the given field. Or, if the field is a section,
-- evaluates pragmas recursively within the fields of the section.
field ::
  (eX :> es, eW :> es) =>
  Exception.Exception E.SomeException eX ->
  Walk.Walk eW ->
  FilePath ->
  Fields.Field (p, Comments.Comments q) ->
  Eff es (Fields.Field (p, Comments.Comments q))
field ex walkH p f = case f of
  Fields.Field n fls -> do
    result <- fieldInner ex walkH p n fls f
    pure $ Maybe.fromMaybe f result
  Fields.Section n sas fs -> Fields.Section n sas <$> traverse (field ex walkH p) fs

fieldInner ::
  (eX :> es, eW :> es) =>
  Exception.Exception E.SomeException eX ->
  Walk.Walk eW ->
  FilePath ->
  Fields.Name (p, Comments.Comments q) ->
  [Fields.FieldLine (p, Comments.Comments q)] ->
  Fields.Field (p, Comments.Comments q) ->
  Eff es (Maybe (Fields.Field (p, Comments.Comments q)))
fieldInner ex walkH p n fls _f = case Map.lookup (Name.value n) relevantFieldNames of
  Nothing -> pure Nothing
  Just dt -> case Utils.safeLast . Comments.toList . snd $ Name.annotation n of
    Nothing -> pure Nothing
    Just comment -> case Parsec.simpleParsecBS $ Comment.value comment of
      Nothing -> pure Nothing
      Just (Pragma.Pragma (Discover ds)) ->
        Just <$> discover ex walkH p n fls dt ds

newtype Discover = Discover [String]

instance Parsec.Parsec Discover where
  parsec = do
    Monad.void $ CharParsing.string "discover"
    arguments <-
      Monad.mplus
        (CharParsing.skipSpaces1 *> CharParsing.sepBy Parsec.parsec CharParsing.skipSpaces1)
        ([] <$ CharParsing.spaces)
    pure . Discover $ fmap Newtypes.getToken' arguments

-- | If modules are discovered for a field, that fields lines are completely
-- replaced.
discover ::
  (eX :> es, eW :> es) =>
  Exception.Exception E.SomeException eX ->
  Walk.Walk eW ->
  FilePath ->
  Fields.Name (p, Comments.Comments q) ->
  [Fields.FieldLine (p, Comments.Comments q)] ->
  DiscoverTarget.DiscoverTarget ->
  [String] ->
  Eff es (Fields.Field (p, Comments.Comments q))
discover ex walkH p n fls dt ds = do
  let (flgs, args, opts, errs) =
        GetOpt.getOpt'
          GetOpt.Permute
          [ GetOpt.Option [] ["include"] (GetOpt.ReqArg Right "PATTERN") "",
            GetOpt.Option [] ["exclude"] (GetOpt.ReqArg Left "PATTERN") ""
          ]
          ds
  let (excs, incs) = Either.partitionEithers flgs
  mapM_ (Exception.throw ex . E.toException . UnknownOption.fromString) opts
  mapM_ (Exception.throw ex . E.toException . InvalidOption.fromString) errs
  let root = FilePath.dropTrailingPathSeparator . clean $ FilePath.takeDirectory p
      directories =
        List.nubOrd
          . fmap clean
          $ if null args then ["."] else args
      exclusions = List.nubOrd $ fmap clean excs
      inclusions =
        List.nubOrd
          . fmap clean
          $ if null incs then fmap (`FilePath.combine` "**") directories else incs
  files <- Walk.walk walkH root inclusions exclusions
  let comments = foldMap (snd . FieldLine.annotation) fls
      position =
        maybe (fst $ Name.annotation n) (fst . FieldLine.annotation) $
          Maybe.listToMaybe fls
      fieldLines = case dt of
        DiscoverTarget.Modules ->
          zipWith ModuleName.toFieldLine ((,) position <$> comments : repeat Comments.empty)
            . Maybe.mapMaybe (toModuleName directories)
            $ Maybe.mapMaybe (stripAnyExtension extensions . clean) files
        DiscoverTarget.Files ->
          zipWith
            (\a -> Fields.FieldLine a . String.toUtf8)
            ((,) position <$> comments : repeat Comments.empty)
            files
      -- This isn't great, but the comments have to go /somewhere/.
      name =
        if null fieldLines
          then Lens.over (Name.annotationLens . Lens._2) (comments <>) n
          else n
  pure $ Fields.Field name fieldLines

-- | Converts separators into POSIX format and then normalizes the result.
clean :: FilePath -> FilePath
clean = FilePath.normalise . FilePath.toPosixSeparators

-- | These are the names of the fields that can have this action applied to
-- them.
relevantFieldNames :: Map.Map Fields.FieldName DiscoverTarget.DiscoverTarget
relevantFieldNames =
  Map.mapKeys String.toUtf8 . Map.fromList $
    [ ("asm-sources", DiscoverTarget.Files),
      ("c-sources", DiscoverTarget.Files),
      ("cxx-sources", DiscoverTarget.Files),
      ("data-files", DiscoverTarget.Files),
      ("exposed-modules", DiscoverTarget.Modules),
      ("extra-doc-files", DiscoverTarget.Files),
      ("extra-source-files", DiscoverTarget.Files),
      ("includes", DiscoverTarget.Files),
      ("install-includes", DiscoverTarget.Files),
      ("js-sources", DiscoverTarget.Files),
      ("license-files", DiscoverTarget.Files),
      ("other-modules", DiscoverTarget.Modules),
      ("signatures", DiscoverTarget.Modules)
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
