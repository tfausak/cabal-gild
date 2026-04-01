module CabalGild.Unstable.Extra.IncludeRenaming where

import qualified CabalGild.Unstable.Extra.ModuleRenaming as ModuleRenaming
import qualified Distribution.Types.IncludeRenaming as IncludeRenaming

sort :: IncludeRenaming.IncludeRenaming -> IncludeRenaming.IncludeRenaming
sort x =
  IncludeRenaming.IncludeRenaming
    { IncludeRenaming.includeProvidesRn = ModuleRenaming.sort $ IncludeRenaming.includeProvidesRn x,
      IncludeRenaming.includeRequiresRn = ModuleRenaming.sort $ IncludeRenaming.includeRequiresRn x
    }
