module CabalGild.Unstable.Warning.DuplicateOption where

import qualified CabalGild.Unstable.Class.Warning as Warning

-- | This warning is emitted when an option is specified more than once.
data DuplicateOption
  = DuplicateOption
  { option :: String,
    old :: String,
    new :: String
  }
  deriving (Eq, Show)

instance Warning.Warning DuplicateOption where
  displayWarning x =
    "overriding option --"
      <> option x
      <> " (before: "
      <> old x
      <> ", after: "
      <> new x
      <> ")"
