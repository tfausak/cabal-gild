module CabalGild.Unstable.Extra.LibraryName where

import qualified CabalGild.Unstable.Extra.UnqualComponentName as UnqualComponentName
import qualified Distribution.Types.LibraryName as LibraryName

toCaseFold :: LibraryName.LibraryName -> LibraryName.LibraryName
toCaseFold ln = case ln of
  LibraryName.LMainLibName -> LibraryName.LMainLibName
  LibraryName.LSubLibName uqn -> LibraryName.LSubLibName $ UnqualComponentName.toCaseFold uqn
