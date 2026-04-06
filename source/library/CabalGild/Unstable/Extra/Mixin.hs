module CabalGild.Unstable.Extra.Mixin where

import qualified CabalGild.Unstable.Extra.IncludeRenaming as IncludeRenaming
import qualified Distribution.Types.Mixin as Mixin

sort :: Mixin.Mixin -> Mixin.Mixin
sort x =
  Mixin.Mixin
    { Mixin.mixinPackageName = Mixin.mixinPackageName x,
      Mixin.mixinLibraryName = Mixin.mixinLibraryName x,
      Mixin.mixinIncludeRenaming = IncludeRenaming.sort $ Mixin.mixinIncludeRenaming x
    }
