module CabalGild.Unstable.Type.Mixin where

import qualified CabalGild.Unstable.Extra.Mixin as Mixin
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.Mixin as Mixin

-- | This is a wrapper around 'Mixin.Mixin' that sorts the nested module names
-- when printing.
newtype Mixin = Mixin
  { unwrap :: Mixin.Mixin
  }
  deriving (Eq, Ord, Show)

instance Parsec.Parsec Mixin where
  parsec = Mixin <$> Parsec.parsec

instance Pretty.Pretty Mixin where
  pretty = Pretty.pretty . Mixin.sort . unwrap
