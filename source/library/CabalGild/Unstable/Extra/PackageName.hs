module CabalGild.Unstable.Extra.PackageName where

import qualified CabalGild.Unstable.Extra.String as String
import qualified Distribution.Types.PackageName as PackageName

toCaseFold :: PackageName.PackageName -> PackageName.PackageName
toCaseFold =
  PackageName.mkPackageName
    . String.toCaseFold
    . PackageName.unPackageName
