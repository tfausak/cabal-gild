module CabalGild.Exception.CheckFailure where

import qualified Control.Monad.Catch as Exception

data CheckFailure
  = CheckFailure
  deriving (Eq, Show)

instance Exception.Exception CheckFailure where
  displayException = const "input is not formatted"
