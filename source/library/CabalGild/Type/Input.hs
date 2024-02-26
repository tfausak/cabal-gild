module CabalGild.Type.Input where

-- | Represents an input stream, which can either be standard input (STDIN) or
-- a file.
data Input
  = Stdin
  | File FilePath
  deriving (Eq, Ord, Show)
