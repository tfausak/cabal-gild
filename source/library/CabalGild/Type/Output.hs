module CabalGild.Type.Output where

-- | Represents an output stream, which can either be standard output
-- (STDOUT) or a file.
data Output
  = Stdout
  | File FilePath
  deriving (Eq, Ord, Show)

-- | Converts a string into an output. The string @"-"@ will be converted into
-- 'Stdout', and any other string will be converted into 'File'.
fromString :: String -> Output
fromString s = case s of
  "-" -> Stdout
  _ -> File s
