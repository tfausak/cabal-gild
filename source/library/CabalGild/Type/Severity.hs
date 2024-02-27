module CabalGild.Type.Severity where

-- | Represents the severity of a log message.
data Severity
  = Info
  | Warn
  deriving (Eq, Show)
