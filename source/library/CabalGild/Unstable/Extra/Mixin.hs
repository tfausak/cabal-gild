module CabalGild.Unstable.Extra.Mixin where

import qualified CabalGild.Unstable.Extra.IncludeRenaming as IncludeRenaming
import qualified CabalGild.Unstable.Extra.LibraryName as LibraryName
import qualified CabalGild.Unstable.Extra.PackageName as PackageName
import qualified Distribution.Types.Mixin as Mixin

sort :: Mixin.Mixin -> Mixin.Mixin
sort x =
  Mixin.Mixin
    { Mixin.mixinPackageName = Mixin.mixinPackageName x,
      Mixin.mixinLibraryName = Mixin.mixinLibraryName x,
      Mixin.mixinIncludeRenaming = IncludeRenaming.sort $ Mixin.mixinIncludeRenaming x
    }

toCaseFold :: Mixin.Mixin -> Mixin.Mixin
toCaseFold x =
  x
    { Mixin.mixinPackageName = PackageName.toCaseFold $ Mixin.mixinPackageName x,
      Mixin.mixinLibraryName = LibraryName.toCaseFold $ Mixin.mixinLibraryName x
    }
