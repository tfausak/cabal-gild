module CabalGild.Unstable.Exception.MoreThanOneCabalFileFound where

import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when no input or output file has been specified,
-- stdin is a terminal device, and there are multiple Cabal files in the
-- current directory.
data MoreThanOneCabalFileFound
  = MoreThanOneCabalFileFound
  deriving (Eq, Show)

instance Exception.Exception MoreThanOneCabalFileFound where
  displayException =
    const $
      "More than one package description found in the current directory. Use the --"
        <> Flag.ioOption
        <> " option to specify a *.cabal file."
