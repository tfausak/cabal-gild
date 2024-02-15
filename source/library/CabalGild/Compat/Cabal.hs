{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CabalGild.Compat.Cabal where

#if !MIN_VERSION_Cabal_syntax(3, 10, 1)

import qualified Distribution.Types.Dependency as Dependency
import qualified Distribution.Types.VersionRange.Internal as VersionRange
import qualified Distribution.Types.ExeDependency as ExeDependency
import qualified Distribution.Types.LegacyExeDependency as LegacyExeDependency
import qualified Distribution.Types.ForeignLibOption as ForeignLibOption
import qualified Distribution.Types.PkgconfigDependency as PkgconfigDependency
import qualified Distribution.Types.PkgconfigVersionRange as PkgconfigVersionRange
import qualified Distribution.Types.ModuleReexport as ModuleReexport
import qualified Language.Haskell.Extension as Extension

deriving instance Ord Dependency.Dependency
deriving instance Ord ForeignLibOption.ForeignLibOption
deriving instance Ord ExeDependency.ExeDependency
deriving instance Ord Extension.Language
deriving instance Ord LegacyExeDependency.LegacyExeDependency
deriving instance Ord ModuleReexport.ModuleReexport
deriving instance Ord PkgconfigDependency.PkgconfigDependency
deriving instance Ord PkgconfigVersionRange.PkgconfigVersionRange
deriving instance Ord VersionRange.VersionRange

#endif
