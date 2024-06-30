module CabalGild.Unstable.Exception.DuplicateOption where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when an option is specified more than once.
data DuplicateOption
  = DuplicateOption
  { option :: String,
    old :: String,
    new :: String
  }
  deriving (Eq, Show)

instance Exception.Exception DuplicateOption where
  displayException x =
    "overriding option --"
      <> option x
      <> " (before: "
      <> old x
      <> ", after: "
      <> new x
      <> ")"
