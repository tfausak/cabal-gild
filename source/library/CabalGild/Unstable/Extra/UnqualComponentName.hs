module CabalGild.Unstable.Extra.UnqualComponentName where

import qualified CabalGild.Unstable.Extra.String as String
import qualified Data.Text as Text
import qualified Distribution.Types.UnqualComponentName as UnqualComponentName

toCaseFold :: UnqualComponentName.UnqualComponentName -> UnqualComponentName.UnqualComponentName
toCaseFold =
  UnqualComponentName.mkUnqualComponentName
    . String.toCaseFold
    . UnqualComponentName.unUnqualComponentName
