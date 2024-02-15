{-# LANGUAGE TypeApplications #-}

module CabalGild.Action.Format where

import qualified CabalGild.Extra.FieldLine as FieldLine
import qualified CabalGild.Extra.Name as Name
import qualified CabalGild.Extra.String as String
import qualified CabalGild.Type.SomeParsecParser as SPP
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Fields as Fields
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.Dependency as Dependency
import qualified Distribution.Types.ExeDependency as ExeDependency
import qualified Distribution.Types.ForeignLibOption as ForeignLibOption
import qualified Distribution.Types.LegacyExeDependency as LegacyExeDependency
import qualified Distribution.Types.Mixin as Mixin
import qualified Distribution.Types.ModuleReexport as ModuleReexport
import qualified Distribution.Types.PkgconfigDependency as PkgconfigDependency
import qualified Language.Haskell.Extension as Extension
import qualified Text.PrettyPrint as PrettyPrint

run ::
  (Applicative m, Monoid cs) =>
  CabalSpecVersion.CabalSpecVersion ->
  ([Fields.Field cs], cs) ->
  m ([Fields.Field cs], cs)
run csv (fs, cs) = pure (fields csv fs, cs)

fields ::
  (Monoid cs) =>
  CabalSpecVersion.CabalSpecVersion ->
  [Fields.Field cs] ->
  [Fields.Field cs]
fields = fmap . field

field ::
  (Monoid cs) =>
  CabalSpecVersion.CabalSpecVersion ->
  Fields.Field cs ->
  Fields.Field cs
field csv f = case f of
  Fields.Field n fls -> case Map.lookup (Name.value n) parsers of
    Nothing -> f
    Just spp -> Fields.Field n $ fieldLines csv fls spp
  Fields.Section n sas fs -> Fields.Section n sas $ fields csv fs

fieldLines ::
  (Monoid cs) =>
  CabalSpecVersion.CabalSpecVersion ->
  [Fields.FieldLine cs] ->
  SPP.SomeParsecParser ->
  [Fields.FieldLine cs]
fieldLines csv fls (SPP.SomeParsecParser pp) =
  case Parsec.runParsecParser' csv pp "" $ FieldLine.toFieldLineStream fls of
    Left _ ->
      -- Parsing failed, so simply return the field lines as is.
      fls
    Right r ->
      fmap (\(c, l) -> Fields.FieldLine c $ String.toUtf8 l)
        . zip (foldMap FieldLine.annotation fls : repeat mempty)
        . lines
        . PrettyPrint.renderStyle style
        $ Pretty.prettyVersioned csv r

style :: PrettyPrint.Style
style =
  -- Everything should be on its own line.
  PrettyPrint.Style
    { PrettyPrint.mode = PrettyPrint.PageMode,
      PrettyPrint.lineLength = 0,
      PrettyPrint.ribbonsPerLine = 1
    }

parsers :: Map.Map Fields.FieldName SPP.SomeParsecParser
parsers =
  -- Perhaps these should use paths as keys rather than field names. That's
  -- because some fields are only supposed to occur within certain sections.
  -- For example, `exposed-modules` occurs in (at least) `library`. Fortunately
  -- field names are unique enough for this not to be a problem.
  let (=:) :: String -> SPP.SomeParsecParser -> (Fields.FieldName, SPP.SomeParsecParser)
      (=:) = (,) . String.toUtf8
   in Map.fromList
        [ "asm-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "asm-sources" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "autogen-includes" =: SPP.set @Newtypes.FSep @Newtypes.FilePathNT,
          "autogen-modules" =: SPP.set @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName),
          "build-depends" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity Dependency.Dependency), -- TODO
          "build-tool-depends" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity ExeDependency.ExeDependency), -- TODO
          "build-tools" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity LegacyExeDependency.LegacyExeDependency), -- TODO
          "c-sources" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "cc-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "cmm-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "cmm-sources" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "code-generators" =: SPP.list @Newtypes.CommaFSep @Newtypes.Token,
          "cpp-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "cxx-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "cxx-sources" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "data-files" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "default-extensions" =: SPP.set @Newtypes.FSep @(Newtypes.MQuoted Extension.Extension),
          "exposed-modules" =: SPP.set @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName),
          "extensions" =: SPP.set @Newtypes.FSep @(Newtypes.MQuoted Extension.Extension),
          "extra-bundled-libraries" =: SPP.set @Newtypes.VCat @Newtypes.Token,
          "extra-doc-files" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "extra-dynamic-library-flavours" =: SPP.set @Newtypes.VCat @Newtypes.Token,
          "extra-framework-dirs" =: SPP.set @Newtypes.FSep @Newtypes.FilePathNT,
          "extra-ghci-libraries" =: SPP.set @Newtypes.VCat @Newtypes.Token,
          "extra-lib-dirs-static" =: SPP.set @Newtypes.FSep @Newtypes.FilePathNT,
          "extra-lib-dirs" =: SPP.set @Newtypes.FSep @Newtypes.FilePathNT,
          "extra-libraries-static" =: SPP.set @Newtypes.VCat @Newtypes.Token,
          "extra-libraries" =: SPP.set @Newtypes.VCat @Newtypes.Token,
          "extra-library-flavours" =: SPP.set @Newtypes.VCat @Newtypes.Token,
          "extra-source-files" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "extra-tmp-files" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "frameworks" =: SPP.set @Newtypes.FSep @Newtypes.Token,
          "ghc-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "ghc-prof-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "ghc-shared-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "ghcjs-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "ghcjs-prof-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "ghcjs-shared-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "hs-source-dirs" =: SPP.list @Newtypes.FSep @Newtypes.FilePathNT,
          "hsc2hs-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "include-dirs" =: SPP.set @Newtypes.FSep @Newtypes.FilePathNT,
          "includes" =: SPP.set @Newtypes.FSep @Newtypes.FilePathNT,
          "install-includes" =: SPP.set @Newtypes.FSep @Newtypes.FilePathNT,
          "js-sources" =: SPP.set @Newtypes.VCat @Newtypes.FilePathNT,
          "ld-options" =: SPP.list @Newtypes.NoCommaFSep @Newtypes.Token',
          "license-files" =: SPP.set @Newtypes.FSep @Newtypes.FilePathNT,
          "mixins" =: SPP.set @Newtypes.CommaVCat @(Identity.Identity Mixin.Mixin),
          "options" =: SPP.list @Newtypes.FSep @(Identity.Identity ForeignLibOption.ForeignLibOption), -- TODO
          "other-extensions" =: SPP.set @Newtypes.FSep @(Newtypes.MQuoted Extension.Extension),
          "other-languages" =: SPP.list @Newtypes.FSep @(Newtypes.MQuoted Extension.Language), -- TODO
          "other-modules" =: SPP.set @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName),
          "pkgconfig-depends" =: SPP.list @Newtypes.CommaFSep @(Identity.Identity PkgconfigDependency.PkgconfigDependency), -- TODO
          "reexported-modules" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity ModuleReexport.ModuleReexport), -- TODO
          "setup-depends" =: SPP.list @Newtypes.CommaVCat @(Identity.Identity Dependency.Dependency), -- TODO
          "signatures" =: SPP.set @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName),
          "tested-with" =: SPP.list @Newtypes.FSep @Newtypes.TestedWith, -- TODO
          "virtual-modules" =: SPP.set @Newtypes.VCat @(Newtypes.MQuoted ModuleName.ModuleName)
        ]
