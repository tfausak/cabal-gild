module CabalGild.Exception.UnknownOption where

import qualified Control.Monad.Catch as Exception

newtype UnknownOption
  = UnknownOption String
  deriving (Eq, Show)

instance Exception.Exception UnknownOption where
  displayException (UnknownOption s) = "unknown option: " <> s

fromString :: String -> UnknownOption
fromString = UnknownOption
