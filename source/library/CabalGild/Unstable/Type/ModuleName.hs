module CabalGild.Unstable.Type.ModuleName where

import qualified CabalGild.Unstable.Extra.ModuleName as ModuleName
import qualified Data.Ord as Ord
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty

-- | This is a wrapper around 'ModuleName.ModuleName' that compares case
-- insensitively.
newtype ModuleName = ModuleName
  { unwrap :: ModuleName.ModuleName
  }
  deriving (Eq, Show)

instance Ord ModuleName where
  compare = Ord.comparing $ \x ->
    let y = unwrap x in (ModuleName.toCaseFold y, y)

instance Parsec.Parsec ModuleName where
  parsec = ModuleName <$> Parsec.parsec

instance Pretty.Pretty ModuleName where
  pretty = Pretty.pretty . unwrap
