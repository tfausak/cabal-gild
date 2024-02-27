module CabalGild.Exception.InvalidConfig where

import qualified Control.Monad.Catch as Exception

data InvalidConfig
  = CheckModeWithOutput
  | FileInputWithStdin
  deriving (Eq, Show)

instance Exception.Exception InvalidConfig where
  displayException x =
    "invalid config: " <> case x of
      CheckModeWithOutput -> "cannot use --output when --mode is check"
      FileInputWithStdin -> "cannot use --stdin when --input is a file"
