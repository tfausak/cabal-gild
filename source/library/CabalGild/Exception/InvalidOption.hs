module CabalGild.Exception.InvalidOption where

import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List

newtype InvalidOption
  = InvalidOption String
  deriving (Eq, Show)

instance Exception.Exception InvalidOption where
  displayException (InvalidOption s) = "invalid option: " <> s

fromString :: String -> InvalidOption
fromString = InvalidOption . List.dropWhileEnd Char.isSpace
