module CabalGild.Unstable.Extra.PackageName where

import qualified Data.Text as Text
import qualified Distribution.Types.PackageName as PackageName

toCaseFold :: PackageName.PackageName -> PackageName.PackageName
toCaseFold =
  PackageName.mkPackageName
    . Text.unpack
    . Text.toCaseFold
    . Text.pack
    . PackageName.unPackageName
