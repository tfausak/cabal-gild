module CabalGild.Unstable.Exception.MixedArgumentStyles where

import qualified Control.Monad.Catch as Exception

newtype MixedArgumentStyles
  = MixedArgumentStyles String
  deriving (Eq, Show)

instance Exception.Exception MixedArgumentStyles where
  displayException (MixedArgumentStyles opt) =
    "cannot use --" <> opt <> " with positional file arguments"
