module CabalGild.Unstable.Type.Input where

-- | Represents an input stream, which can either be standard input (STDIN) or
-- a file.
data Input
  = Stdin
  | File FilePath
  deriving (Eq, Ord, Show)

-- | Converts a string into an input. The string @"-"@ will be converted into
-- 'Stdin', and any other string will be converted into 'File'.
fromString :: String -> Input
fromString s = case s of
  "-" -> Stdin
  _ -> File s
