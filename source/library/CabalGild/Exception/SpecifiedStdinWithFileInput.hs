module CabalGild.Exception.SpecifiedStdinWithFileInput where

import qualified Control.Monad.Catch as Exception

data SpecifiedStdinWithFileInput
  = SpecifiedStdinWithFileInput
  deriving (Eq, Show)

instance Exception.Exception SpecifiedStdinWithFileInput where
  displayException = const "cannot use --stdin when --input is a file"
