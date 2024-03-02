module CabalGild.Unstable.Exception.InvalidLeniency where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when the given
-- 'CabalGild.Type.Leniency.Leniency' is invalid.
newtype InvalidLeniency
  = InvalidLeniency String
  deriving (Eq, Show)

instance Exception.Exception InvalidLeniency where
  displayException (InvalidLeniency s) = "invalid leniency: " <> s

-- | Constructs an 'InvalidLeniency' from the given 'String'.
fromString :: String -> InvalidLeniency
fromString = InvalidLeniency
