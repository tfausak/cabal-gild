module CabalGild.Type.Output where

-- | Represents an output stream, which can either be standard output
-- (STDOUT) or a file.
data Output
  = Stdout
  | File FilePath
  deriving (Eq, Ord, Show)
