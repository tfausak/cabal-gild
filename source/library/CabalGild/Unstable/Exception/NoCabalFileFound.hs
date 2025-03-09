module CabalGild.Unstable.Exception.NoCabalFileFound where

import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when no input or output file has been specified,
-- stdin is a terminal device, and there are no Cabal files in the current
-- directory.
data NoCabalFileFound
  = NoCabalFileFound
  deriving (Eq, Show)

instance Exception.Exception NoCabalFileFound where
  displayException =
    const $
      "No package descriptions found in the current directory. Use the --"
        <> Flag.ioOption
        <> " option to specify a *.cabal file."
