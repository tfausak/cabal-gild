module CabalGild.Unstable.Exception.MixedArgumentStyles where

import qualified Control.Exception as Exception

-- | This exception is thrown when positional file arguments are used together
-- with the deprecated @--input@, @--output@, or @--io@ flags.
newtype MixedArgumentStyles
  = MixedArgumentStyles String
  deriving (Eq, Show)

instance Exception.Exception MixedArgumentStyles where
  displayException (MixedArgumentStyles opt) =
    "cannot use --" <> opt <> " with positional file arguments"
