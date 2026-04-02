module CabalGild.Unstable.Extra.FilePath where

-- | Converts a 'FilePath' with potential Windows-style separators in it to one
-- with only POSIX-style separators.
toPosixSeparators :: FilePath -> FilePath
toPosixSeparators = fmap $ \c -> case c of
  '\\' -> '/'
  _ -> c
