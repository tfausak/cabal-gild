module CabalGild.Unstable.Exception.InvalidOption where

import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List

-- | This exception is thrown when a command line option is invalid.
newtype InvalidOption
  = InvalidOption String
  deriving (Eq, Show)

instance Exception.Exception InvalidOption where
  displayException (InvalidOption s) = "invalid option: " <> s

-- | Constructs an 'InvalidOption' from the given 'String'.
fromString :: String -> InvalidOption
fromString = InvalidOption . List.dropWhileEnd Char.isSpace
