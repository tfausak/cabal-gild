{-# LANGUAGE TypeApplications #-}

module CabalGild.Unstable.Action.FormatFields where

import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.UnqualComponentName as UnqualComponentName
import qualified Data.Containers.ListUtils as ListUtils
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.PackageName as PackageName
import qualified CabalGild.Unstable.Extra.SectionArg as SectionArg
import qualified CabalGild.Unstable.Extra.String as String
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Condition as Condition
import qualified CabalGild.Unstable.Type.Dependency as Dependency
import qualified CabalGild.Unstable.Type.Extension as Extension
import qualified CabalGild.Unstable.Type.Mixin as Mixin
import qualified CabalGild.Unstable.Type.ModuleName as ModuleName
import qualified Distribution.ModuleName as ModuleName2
import qualified CabalGild.Unstable.Type.SomeParsecParser as SPP
import qualified CabalGild.Unstable.Type.TestedWith as TestedWith
import qualified CabalGild.Unstable.Type.Variable as Variable
import qualified Data.ByteString as ByteString
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Function as Function
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Parsec.FieldLineStream as FieldLineStream
import qualified Distribution.Types.ExeDependency as ExeDependency
import qualified Distribution.Types.ForeignLibOption as ForeignLibOption
import qualified Distribution.Types.LegacyExeDependency as LegacyExeDependency
import qualified Distribution.Types.ModuleReexport as ModuleReexport
import qualified Distribution.Types.PkgconfigDependency as PkgconfigDependency
import qualified Language.Haskell.Extension as Haskell
import qualified Text.PrettyPrint as PrettyPrint
import qualified Distribution.Compat.Newtype as Newtype

-- | A wrapper around 'field' to allow this to be composed with other actions.
run ::
  (Applicative m) =>
  CabalSpecVersion.CabalSpecVersion ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  m ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run csv (fs, cs) = pure (fmap (field csv) fs, cs)

-- | Formats the given field, if applicable. Otherwise returns the field as is.
-- If the field is a section, the fields within the section will be recursively
-- formatted.
field ::
  CabalSpecVersion.CabalSpecVersion ->
  Fields.Field (p, Comments.Comments q) ->
  Fields.Field (p, Comments.Comments q)
field csv f = case f of
  Fields.Field n fls ->
    let position =
          maybe (fst $ Name.annotation n) (fst . FieldLine.annotation) $
            Maybe.listToMaybe fls
     in Fields.Field n $ case Map.lookup (Name.value n) parsers of
          Nothing -> floatComments position fls
          Just spp -> fieldLines csv position fls spp
  Fields.Section n sas fs ->
    let result =
          Parsec.runParsecParser' csv (Condition.parseCondition Variable.parseVariable) "<conditional>"
            . FieldLineStream.fieldLineStreamFromBS
            . joinSectionArgs
            $ fmap SectionArg.value sas
        position =
          fst
            . maybe (Name.annotation n) SectionArg.annotation
            $ Maybe.listToMaybe sas
        newSas =
          if isConditional csv n
            then case result of
              Left _ -> sas
              Right c ->
                pure
                  . Fields.SecArgName (position, Comments.empty)
                  . String.toUtf8
                  . PrettyPrint.renderStyle style
                  $ Condition.prettyCondition Variable.prettyVariable c
            else sas
     in Fields.Section n newSas $ fmap (field csv) fs

-- | Joins section argument values with spaces, but concatenates a @*@ wildcard
-- directly onto a preceding token that ends with @.@ to preserve version
-- wildcards like @9.10.*@.
joinSectionArgs :: [ByteString.ByteString] -> ByteString.ByteString
joinSectionArgs =
  let space = ByteString.singleton 0x20
      asterisk = ByteString.singleton 0x2a
      fullStop = ByteString.singleton 0x2e
      merge = Function.fix $ \rec xs -> case xs of
        x : y : ys
          | ByteString.isSuffixOf fullStop x,
            y == asterisk ->
              (x <> y) : rec ys
          | otherwise -> x : rec (y : ys)
        _ -> xs
   in ByteString.intercalate space . merge

-- | Returns 'True' if the field name is a conditional. @if@ is always one, and
-- @elif@ is one for Cabal versions 2.2 and later.
isConditional :: CabalSpecVersion.CabalSpecVersion -> Fields.Name p -> Bool
isConditional csv n =
  Name.isIf n
    || Name.isElif csv n

-- | Attempts to parse the given field lines using the given parser. If parsing
-- fails, the field lines will be returned as is. Comments within the field
-- lines will be preserved but "float" up to the top.
fieldLines ::
  CabalSpecVersion.CabalSpecVersion ->
  p ->
  [Fields.FieldLine (p, Comments.Comments q)] ->
  SPP.SomeParsecParser ->
  [Fields.FieldLine (p, Comments.Comments q)]
fieldLines csv position fls SPP.SomeParsecParser {SPP.parsec = parsec, SPP.pretty = pretty} =
  case Parsec.runParsecParser' csv parsec "" $ FieldLine.toFieldLineStream fls of
    Left _ -> floatComments position fls
    Right r ->
      zipWith
        (\b l -> Fields.FieldLine (position, if b then collectComments fls else Comments.empty) $ String.toUtf8 l)
        (True : repeat False)
        . lines
        . PrettyPrint.renderStyle style
        $ pretty csv r

-- | Collects comments from the given field lines (see 'collectComments') and
-- attaches them all to the first one.
floatComments ::
  p ->
  [Fields.FieldLine (p, Comments.Comments q)] ->
  [Fields.FieldLine (p, Comments.Comments q)]
floatComments p fls =
  zipWith
    (\b -> Fields.FieldLine (p, if b then collectComments fls else Comments.empty) . FieldLine.value)
    (True : repeat False)
    fls

-- | Collects all comments from the given field lines. Their relative order
-- will be maintained.
collectComments :: [Fields.FieldLine (p, Comments.Comments q)] -> Comments.Comments q
collectComments = foldMap (snd . FieldLine.annotation)

-- | This style attempts to force everything to be on its own line.
style :: PrettyPrint.Style
style =
  PrettyPrint.Style
    { PrettyPrint.mode = PrettyPrint.PageMode,
      PrettyPrint.lineLength = 0,
      PrettyPrint.ribbonsPerLine = 1
    }

-- | A map from field names to parsers. This determines which parser should be
-- used for which field. And consequently this determines which fields will be
-- formatted.
--
-- Perhaps instead of being keyed on 'Fields.FieldName', this should be keyed
-- on a path (list of field names) instead. That's because a field like
-- @build-depends@ only really makes sense within a section like @library@.
-- Fortunately field names are unique enough that this hasn't been a problem
-- yet.
parsers :: Map.Map Fields.FieldName SPP.SomeParsecParser
parsers =
  let (=:) :: String -> SPP.SomeParsecParser -> (Fields.FieldName, SPP.SomeParsecParser)
      (=:) = (,) . String.toUtf8
   in Map.fromList
        [ "asm-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "asm-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sortOn (Text.toCaseFold . Text.pack) . ListUtils.nubOrd . Newtype.unpack),
          "autogen-includes" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT (Newtype.pack . List.sortOn (Text.toCaseFold . Text.pack) . ListUtils.nubOrd . Newtype.unpack),
          "autogen-modules" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName2.ModuleName) (Newtype.pack . List.sortOn (fmap (Text.toCaseFold . Text.pack) . ModuleName2.components) . ListUtils.nubOrd . Newtype.unpack),
          "build-depends" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity Dependency.Dependency) (Newtype.pack . List.sortOn (\ d -> (PackageName.toCaseFold $ Dependency.packageName d, fmap (Bifunctor.bimap UnqualComponentName.toCaseFold (fmap UnqualComponentName.toCaseFold)) $ Dependency.libraryNames d, Dependency.versionRange d)) . ListUtils.nubOrd . Newtype.unpack),
          -- TODO: continue
          "build-tool-depends" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity ExeDependency.ExeDependency) (Newtype.pack . List.sort . Newtype.unpack),
          "build-tools" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity LegacyExeDependency.LegacyExeDependency) (Newtype.pack . List.sort . Newtype.unpack),
          "c-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "cc-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "cmm-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "cmm-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "code-generators" =: SPP.list @Newtypes.CommaFSep @Newtypes.Token id,
          "cpp-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "cxx-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "cxx-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "data-files" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "default-extensions" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Extension.Extension) (Newtype.pack . List.sort . Newtype.unpack),
          "exposed-modules" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) (Newtype.pack . List.sort . Newtype.unpack),
          "extensions" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Extension.Extension) (Newtype.pack . List.sort . Newtype.unpack),
          "extra-bundled-libraries" =: SPP.list @Newtypes.VCat @Newtypes.Token (Newtype.pack . List.sort . Newtype.unpack),
          "extra-doc-files" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "extra-dynamic-library-flavours" =: SPP.list @Newtypes.VCat @Newtypes.Token (Newtype.pack . List.sort . Newtype.unpack),
          "extra-framework-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "extra-ghci-libraries" =: SPP.list @Newtypes.VCat @Newtypes.Token (Newtype.pack . List.sort . Newtype.unpack),
          "extra-lib-dirs-static" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "extra-lib-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "extra-libraries-static" =: SPP.list @Newtypes.VCat @Newtypes.Token (Newtype.pack . List.sort . Newtype.unpack),
          "extra-libraries" =: SPP.list @Newtypes.VCat @Newtypes.Token (Newtype.pack . List.sort . Newtype.unpack),
          "extra-library-flavours" =: SPP.list @Newtypes.VCat @Newtypes.Token (Newtype.pack . List.sort . Newtype.unpack),
          "extra-source-files" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "extra-tmp-files" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "frameworks" =: SPP.list @Newtypes.FSep @Newtypes.Token (Newtype.pack . List.sort . Newtype.unpack),
          "ghc-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghc-prof-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghc-shared-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghcjs-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghcjs-prof-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghcjs-shared-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "hs-source-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT id,
          "hsc2hs-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "include-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "includes" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "install-includes" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "js-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "ld-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "license-files" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT (Newtype.pack . List.sort . Newtype.unpack),
          "mixins" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity Mixin.Mixin) (Newtype.pack . List.sort . Newtype.unpack),
          "options" =: SPP.list @Newtypes.FSep @(Identity.Identity ForeignLibOption.ForeignLibOption) (Newtype.pack . List.sort . Newtype.unpack),
          "other-extensions" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Extension.Extension) (Newtype.pack . List.sort . Newtype.unpack),
          "other-languages" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Haskell.Language) (Newtype.pack . List.sort . Newtype.unpack),
          "other-modules" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) (Newtype.pack . List.sort . Newtype.unpack),
          "pkgconfig-depends" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity PkgconfigDependency.PkgconfigDependency) (Newtype.pack . List.sort . Newtype.unpack),
          "reexported-modules" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity ModuleReexport.ModuleReexport) (Newtype.pack . List.sort . Newtype.unpack),
          "setup-depends" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity Dependency.Dependency) (Newtype.pack . List.sort . Newtype.unpack),
          "signatures" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) (Newtype.pack . List.sort . Newtype.unpack),
          "tested-with" =: SPP.list @Newtypes.FSep @(Identity.Identity TestedWith.TestedWith) (Newtype.pack . List.sort . Newtype.unpack),
          "virtual-modules" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) (Newtype.pack . List.sort . Newtype.unpack)
        ]
