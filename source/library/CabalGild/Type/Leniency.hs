module CabalGild.Type.Leniency where

import qualified CabalGild.Exception.InvalidLeniency as InvalidLeniency
import qualified Control.Monad.Catch as Exception

-- | TODO
data Leniency
  = Lenient
  | Strict
  deriving (Eq, Show)

-- | TODO
fromString :: (Exception.MonadThrow m) => String -> m Leniency
fromString s = case s of
  "lenient" -> pure Lenient
  "strict" -> pure Strict
  _ -> Exception.throwM $ InvalidLeniency.fromString s
