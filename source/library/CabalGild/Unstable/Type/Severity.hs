module CabalGild.Unstable.Type.Severity where

-- | Which level to log a message at.
data Severity
  = Info
  | Warn
  deriving (Eq, Show)
