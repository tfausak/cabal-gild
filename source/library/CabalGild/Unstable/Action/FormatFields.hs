{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module CabalGild.Unstable.Action.FormatFields where

import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.SectionArg as SectionArg
import qualified CabalGild.Unstable.Extra.String as String
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Condition as Condition
import qualified CabalGild.Unstable.Type.Dependency as Dependency
import qualified CabalGild.Unstable.Type.Mixin as Mixin
import qualified CabalGild.Unstable.Type.SomeParsecParser as SPP
import qualified CabalGild.Unstable.Type.TestedWith as TestedWith
import qualified CabalGild.Unstable.Type.Variable as Variable
import qualified Data.ByteString as ByteString
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Function as Function
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.Compat.Newtype as Newtype
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Fields as Fields
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Parsec.FieldLineStream as FieldLineStream
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.ExeDependency as ExeDependency
import qualified Distribution.Types.ForeignLibOption as ForeignLibOption
import qualified Distribution.Types.LegacyExeDependency as LegacyExeDependency
import qualified Distribution.Types.ModuleReexport as ModuleReexport
import qualified Distribution.Types.PkgconfigDependency as PkgconfigDependency
import qualified Language.Haskell.Extension as Haskell
import qualified Text.PrettyPrint as PrettyPrint

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
          "asm-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "autogen-includes" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT stringSet,
          "autogen-modules" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) prettySet,
          "build-depends" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity Dependency.Dependency) prettySet,
          "build-tool-depends" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity ExeDependency.ExeDependency) prettySet,
          "build-tools" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity LegacyExeDependency.LegacyExeDependency) prettySet,
          "c-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "cc-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "cmm-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "cmm-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "code-generators" =: SPP.list @Newtypes.CommaFSep @Newtypes.Token id,
          "cpp-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "cxx-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "cxx-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "data-files" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "default-extensions" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Haskell.Extension) prettySet,
          "exposed-modules" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) prettySet,
          "extensions" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Haskell.Extension) prettySet,
          "extra-bundled-libraries" =: SPP.list @Newtypes.VCat @Newtypes.Token stringSet,
          "extra-doc-files" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "extra-dynamic-library-flavours" =: SPP.list @Newtypes.VCat @Newtypes.Token stringSet,
          "extra-framework-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT stringSet,
          "extra-ghci-libraries" =: SPP.list @Newtypes.VCat @Newtypes.Token stringSet,
          "extra-lib-dirs-static" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT stringSet,
          "extra-lib-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT stringSet,
          "extra-libraries-static" =: SPP.list @Newtypes.VCat @Newtypes.Token stringSet,
          "extra-libraries" =: SPP.list @Newtypes.VCat @Newtypes.Token stringSet,
          "extra-library-flavours" =: SPP.list @Newtypes.VCat @Newtypes.Token stringSet,
          "extra-source-files" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "extra-tmp-files" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "frameworks" =: SPP.list @Newtypes.FSep @Newtypes.Token stringSet,
          "ghc-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghc-prof-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghc-shared-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghcjs-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghcjs-prof-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "ghcjs-shared-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "hs-source-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT id,
          "hsc2hs-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "include-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT stringSet,
          "includes" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT stringSet,
          "install-includes" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT stringSet,
          "js-sources" =: SPP.list @Newtypes.VCat @Newtypes.FilePathNT stringSet,
          "ld-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token' id,
          "license-files" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT stringSet,
          "mixins" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity Mixin.Mixin) prettySet,
          "options" =: SPP.list @Newtypes.FSep @(Identity.Identity ForeignLibOption.ForeignLibOption) prettySet,
          "other-extensions" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Haskell.Extension) prettySet,
          "other-languages" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Haskell.Language) prettySet,
          "other-modules" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) prettySet,
          "pkgconfig-depends" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity PkgconfigDependency.PkgconfigDependency) prettySet,
          "reexported-modules" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity ModuleReexport.ModuleReexport) prettySet,
          "setup-depends" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity Dependency.Dependency) prettySet,
          "signatures" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) prettySet,
          "tested-with" =: SPP.list @Newtypes.FSep @(Identity.Identity TestedWith.TestedWith) prettySet,
          "virtual-modules" =: SPP.list @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName) prettySet
        ]

stringSet :: (Newtype.Newtype [String] a) => a -> a
stringSet =
  Newtype.pack
    . List.sortOn String.toCaseFold
    . ListUtils.nubOrd
    . Newtype.unpack

prettySet :: (Newtype.Newtype [b] a, Ord b, Pretty.Pretty b) => a -> a
prettySet =
  Newtype.pack
    . List.sortOn (String.toCaseFold . Pretty.prettyShow)
    . ListUtils.nubOrd
    . Newtype.unpack
