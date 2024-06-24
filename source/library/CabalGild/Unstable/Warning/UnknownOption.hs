module CabalGild.Unstable.Warning.UnknownOption where

import qualified CabalGild.Unstable.Class.Warning as Warning

-- | This warning is thrown when a command line option is not known.
newtype UnknownOption
  = UnknownOption String
  deriving (Eq, Show)

instance Warning.Warning UnknownOption where
  displayWarning (UnknownOption s) = "unknown option: " <> s

-- | Constructs an 'UnknownOption' from the given 'String'.
fromString :: String -> UnknownOption
fromString = UnknownOption
