module CabalGild.Unstable.Exception.SpecifiedCrlfWithFormatMode where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when the user specifies the CRLF leniency while
-- using the format mode. In other words, when @--mode=format@ and
-- @--crlf=anything@.
data SpecifiedCrlfWithFormatMode
  = SpecifiedCrlfWithFormatMode
  deriving (Eq, Show)

instance Exception.Exception SpecifiedCrlfWithFormatMode where
  displayException = const "cannot use --crlf when --mode is format"
