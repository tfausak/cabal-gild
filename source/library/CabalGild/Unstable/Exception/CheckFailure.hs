module CabalGild.Unstable.Exception.CheckFailure where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when the input is not formatted correctly.
data CheckFailure
  = CheckFailure
  deriving (Eq, Show)

instance Exception.Exception CheckFailure where
  displayException = const "input is not formatted"
