module CabalGild.Unstable.Type.ExeDependency where

import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.ExeDependency as ExeDependency

-- | This type exists to provide an 'Ord' instance for
-- 'ExeDependency.ExeDependency', which was added in @Cabal-syntax-3.10.1.0@.
newtype ExeDependency = ExeDependency
  { unwrap :: ExeDependency.ExeDependency
  }
  deriving (Eq, Show)

instance Ord ExeDependency where
  compare =
    Ord.comparing $
      (\(ExeDependency.ExeDependency pn ucn vr) -> (pn, ucn, vr))
        . unwrap

instance Parsec.Parsec ExeDependency where
  parsec = ExeDependency <$> Parsec.parsec

instance Pretty.Pretty ExeDependency where
  pretty = Pretty.pretty . unwrap
