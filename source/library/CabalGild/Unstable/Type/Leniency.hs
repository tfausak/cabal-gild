module CabalGild.Unstable.Type.Leniency where

import qualified CabalGild.Unstable.Exception.InvalidLeniency as InvalidLeniency
import qualified Control.Monad.Catch as Exception

-- | Represents the leniency of a setting. In other words, should something be
-- lenient\/permissive or strict\/pedantic?
data Leniency
  = Lenient
  | Strict
  deriving (Eq, Show)

-- | Attempts to parse a string as a 'Leniency'.
fromString :: (Exception.MonadThrow m) => String -> m Leniency
fromString s = case s of
  "lenient" -> pure Lenient
  "strict" -> pure Strict
  _ -> Exception.throwM $ InvalidLeniency.fromString s
