module CabalGild.Unstable.Exception.InvalidMode where

import qualified Control.Exception as Exception

-- | This exception is thrown when the given 'CabalGild.Type.Mode.Mode' is
-- invalid.
newtype InvalidMode
  = InvalidMode String
  deriving (Eq, Show)

instance Exception.Exception InvalidMode where
  displayException (InvalidMode s) = "invalid mode: " <> s

-- | Constructs an 'InvalidMode' from the given 'String'.
fromString :: String -> InvalidMode
fromString = InvalidMode
