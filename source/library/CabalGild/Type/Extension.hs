module CabalGild.Type.Extension where

import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Language.Haskell.Extension as Extension

-- | This type exists to provide a different 'Ord' instance for
-- 'Extension.Extension'. The instance provided by @Cabal-syntax@ sorts things
-- in a surprising order. This one sorts things alphabetically.
newtype Extension = Extension
  { unwrap :: Extension.Extension
  }
  deriving (Eq, Show)

instance Ord Extension where
  compare = Ord.comparing Pretty.prettyShow

instance Parsec.Parsec Extension where
  parsec = Extension <$> Parsec.parsec

instance Pretty.Pretty Extension where
  pretty = Pretty.pretty . unwrap
