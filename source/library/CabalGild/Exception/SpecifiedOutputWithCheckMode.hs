module CabalGild.Exception.SpecifiedOutputWithCheckMode where

import qualified Control.Monad.Catch as Exception

data SpecifiedOutputWithCheckMode
  = SpecifiedOutputWithCheckMode
  deriving (Eq, Show)

instance Exception.Exception SpecifiedOutputWithCheckMode where
  displayException = const "cannot use --output when --mode is check"
