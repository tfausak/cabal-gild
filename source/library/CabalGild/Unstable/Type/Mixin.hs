module CabalGild.Unstable.Type.Mixin where

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.IncludeRenaming as IncludeRenaming
import qualified Distribution.Types.Mixin as Mixin
import qualified Distribution.Types.ModuleRenaming as ModuleRenaming

-- | This is a wrapper around 'Mixin.Mixin' that sorts the nested module names
-- when parsing.
newtype Mixin = Mixin
  { unwrap :: Mixin.Mixin
  }
  deriving (Eq, Show)

instance Ord Mixin where
  compare = Ord.comparing unwrap

instance Parsec.Parsec Mixin where
  parsec = Mixin . sortMixin <$> Parsec.parsec

instance Pretty.Pretty Mixin where
  pretty = Pretty.pretty . unwrap

-- | Sorts the 'Mixin.mixinIncludeRenaming' field using 'sortIncludeRenaming'.
sortMixin :: Mixin.Mixin -> Mixin.Mixin
sortMixin x =
  Mixin.Mixin
    { Mixin.mixinPackageName = Mixin.mixinPackageName x,
      Mixin.mixinLibraryName = Mixin.mixinLibraryName x,
      Mixin.mixinIncludeRenaming = sortIncludeRenaming $ Mixin.mixinIncludeRenaming x
    }

-- | Sorts both 'IncludeRenaming.includeProvidesRn' and
-- 'IncludeRenaming.includeRequiresRn' fields using 'sortModuleRenaming'.
sortIncludeRenaming :: IncludeRenaming.IncludeRenaming -> IncludeRenaming.IncludeRenaming
sortIncludeRenaming x =
  IncludeRenaming.IncludeRenaming
    { IncludeRenaming.includeProvidesRn = sortModuleRenaming $ IncludeRenaming.includeProvidesRn x,
      IncludeRenaming.includeRequiresRn = sortModuleRenaming $ IncludeRenaming.includeRequiresRn x
    }

-- | Sorts both 'ModuleRenaming.HidingRenaming' and
-- 'ModuleRenaming.ModuleRenaming' variants.
sortModuleRenaming :: ModuleRenaming.ModuleRenaming -> ModuleRenaming.ModuleRenaming
sortModuleRenaming x = case x of
  ModuleRenaming.DefaultRenaming -> ModuleRenaming.DefaultRenaming
  ModuleRenaming.HidingRenaming ys -> ModuleRenaming.HidingRenaming $ List.sort ys
  ModuleRenaming.ModuleRenaming ys -> ModuleRenaming.ModuleRenaming $ List.sort ys
