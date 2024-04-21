module CabalGild.Unstable.Exception.InvalidModuleName where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when the given
-- 'Distribution.ModuleName.ModuleName' is invalid.
newtype InvalidModuleName
  = InvalidModuleName String
  deriving (Eq, Show)

instance Exception.Exception InvalidModuleName where
  displayException (InvalidModuleName s) = "invalid module name: " <> s

-- | Constructs an 'InvalidModuleName' from the given 'String'.
fromString :: String -> InvalidModuleName
fromString = InvalidModuleName
