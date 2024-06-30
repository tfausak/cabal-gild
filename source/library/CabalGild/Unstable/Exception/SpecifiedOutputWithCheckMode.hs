module CabalGild.Unstable.Exception.SpecifiedOutputWithCheckMode where

import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when the user specifies the output while using
-- the check mode. In other words, when @--mode=check@ and @--output=anything@.
data SpecifiedOutputWithCheckMode
  = SpecifiedOutputWithCheckMode
  deriving (Eq, Show)

instance Exception.Exception SpecifiedOutputWithCheckMode where
  displayException =
    const $
      "cannot use --"
        <> Flag.outputOption
        <> " when --"
        <> Flag.modeOption
        <> " is check"
