module CabalGild.Unstable.Extra.FilePath where

import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows

-- | Converts a 'FilePath' with potential Windows-style separators in it to one
-- with only POSIX-style separators.
toPosixSeparators :: FilePath -> FilePath
toPosixSeparators = Posix.joinPath . Windows.splitDirectories
