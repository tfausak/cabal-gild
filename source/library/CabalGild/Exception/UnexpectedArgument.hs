module CabalGild.Exception.UnexpectedArgument where

import qualified Control.Monad.Catch as Exception

newtype UnexpectedArgument
  = UnexpectedArgument String
  deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument where
  displayException (UnexpectedArgument s) = "unexpected argument: " <> s

fromString :: String -> UnexpectedArgument
fromString = UnexpectedArgument
