module CabalGild.Unstable.Extra.ModuleRenaming where

import qualified CabalGild.Unstable.Extra.ModuleName as ModuleName
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Distribution.Types.ModuleRenaming as ModuleRenaming

sort :: ModuleRenaming.ModuleRenaming -> ModuleRenaming.ModuleRenaming
sort x = case x of
  ModuleRenaming.DefaultRenaming -> ModuleRenaming.DefaultRenaming
  ModuleRenaming.HidingRenaming ys -> ModuleRenaming.HidingRenaming $ List.sortOn ModuleName.toCaseFold ys
  ModuleRenaming.ModuleRenaming ys -> ModuleRenaming.ModuleRenaming $ List.sortOn (Bifunctor.bimap ModuleName.toCaseFold ModuleName.toCaseFold) ys
