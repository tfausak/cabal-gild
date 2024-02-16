module CabalGild.Exception.UnknownOption where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when a command line option is not known.
newtype UnknownOption
  = UnknownOption String
  deriving (Eq, Show)

instance Exception.Exception UnknownOption where
  displayException (UnknownOption s) = "unknown option: " <> s

-- | Constructs an 'UnknownOption' from the given 'String'.
fromString :: String -> UnknownOption
fromString = UnknownOption
