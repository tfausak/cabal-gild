module CabalGild.Exception.UnexpectedArgument where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when an unexpected command line argument is
-- encountered.
newtype UnexpectedArgument
  = UnexpectedArgument String
  deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument where
  displayException (UnexpectedArgument s) = "unexpected argument: " <> s

-- | Constructs an 'UnexpectedArgument' from the given 'String'.
fromString :: String -> UnexpectedArgument
fromString = UnexpectedArgument
