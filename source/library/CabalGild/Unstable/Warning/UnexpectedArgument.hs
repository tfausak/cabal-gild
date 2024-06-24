module CabalGild.Unstable.Warning.UnexpectedArgument where

import qualified CabalGild.Unstable.Class.Warning as Warning

-- | This warning is thrown when an unexpected command line argument is
-- encountered.
newtype UnexpectedArgument
  = UnexpectedArgument String
  deriving (Eq, Show)

instance Warning.Warning UnexpectedArgument where
  displayWarning (UnexpectedArgument s) = "unexpected argument: " <> s

-- | Constructs an 'UnexpectedArgument' from the given 'String'.
fromString :: String -> UnexpectedArgument
fromString = UnexpectedArgument
