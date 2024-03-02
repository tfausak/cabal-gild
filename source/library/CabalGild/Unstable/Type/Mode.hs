module CabalGild.Unstable.Type.Mode where

import qualified CabalGild.Unstable.Exception.InvalidMode as InvalidMode
import qualified Control.Monad.Catch as Exception

-- | Represents the mode of the command line utility.
data Mode
  = -- | Just determine if the input is already formatted.
    Check
  | -- | Format the input.
    Format
  deriving (Eq, Show)

-- | Attempts to parse a string as a mode.
fromString :: (Exception.MonadThrow m) => String -> m Mode
fromString s = case s of
  "check" -> pure Check
  "format" -> pure Format
  _ -> Exception.throwM $ InvalidMode.fromString s
