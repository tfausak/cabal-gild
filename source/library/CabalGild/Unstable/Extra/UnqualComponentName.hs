module CabalGild.Unstable.Extra.UnqualComponentName where

import qualified Data.Text as Text
import qualified Distribution.Types.UnqualComponentName as UnqualComponentName

toCaseFold :: UnqualComponentName.UnqualComponentName -> UnqualComponentName.UnqualComponentName
toCaseFold =
  UnqualComponentName.mkUnqualComponentName
    . Text.unpack
    . Text.toCaseFold
    . Text.pack
    . UnqualComponentName.unUnqualComponentName
