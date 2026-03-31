module CabalGild.Unstable.Type.Mixin where

import qualified CabalGild.Unstable.Extra.Mixin as Mixin
import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.Mixin as Mixin

-- | This is a wrapper around 'Mixin.Mixin' that compares case insensitively
-- and sorts the nested module names when printing.
newtype Mixin = Mixin
  { unwrap :: Mixin.Mixin
  }
  deriving (Eq, Show)

instance Ord Mixin where
  compare = Ord.comparing $ Mixin.toCaseFold . unwrap

instance Parsec.Parsec Mixin where
  parsec = Mixin <$> Parsec.parsec

instance Pretty.Pretty Mixin where
  pretty = Pretty.pretty . Mixin.sort . unwrap
