module CabalGild.Unstable.Exception.SpecifiedStdinWithFileInput where

import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when the user specifies the STDIN name while
-- using an input file. In other words, when @--input=file@ and
-- @--stdin=anything@.
data SpecifiedStdinWithFileInput
  = SpecifiedStdinWithFileInput
  deriving (Eq, Show)

instance Exception.Exception SpecifiedStdinWithFileInput where
  displayException =
    const $
      "cannot use --"
        <> Flag.stdinOption
        <> " when --"
        <> Flag.inputOption
        <> " is a file"
