module CabalGild.Unstable.Exception.SpecifiedStdinWithFileInput where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when the user specifies the STDIN name while
-- using an input file. In other words, when @--input=file@ and
-- @--stdin=anything@.
data SpecifiedStdinWithFileInput
  = SpecifiedStdinWithFileInput
  deriving (Eq, Show)

instance Exception.Exception SpecifiedStdinWithFileInput where
  displayException = const "cannot use --stdin when --input is a file"
