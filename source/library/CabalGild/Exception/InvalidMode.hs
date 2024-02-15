module CabalGild.Exception.InvalidMode where

import qualified Control.Monad.Catch as Exception

newtype InvalidMode
  = InvalidMode String
  deriving (Eq, Show)

instance Exception.Exception InvalidMode where
  displayException (InvalidMode s) = "invalid mode: " <> s

fromString :: String -> InvalidMode
fromString = InvalidMode
