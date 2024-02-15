module CabalGild.Type.Mode where

import qualified CabalGild.Exception.InvalidMode as InvalidMode
import qualified Control.Monad.Catch as Exception

data Mode
  = Check
  | Format
  deriving (Eq, Show)

fromString :: (Exception.MonadThrow m) => String -> m Mode
fromString s = case s of
  "check" -> pure Check
  "format" -> pure Format
  _ -> Exception.throwM $ InvalidMode.fromString s
