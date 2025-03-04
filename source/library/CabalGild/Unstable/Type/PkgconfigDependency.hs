module CabalGild.Unstable.Type.PkgconfigDependency where

import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.PkgconfigDependency as PkgconfigDependency

-- | This type exists to provide an 'Ord' instance for
-- 'PkgconfigDependency.PkgconfigDependency', which was added in
-- @Cabal-syntax-3.10.1.0@.
newtype PkgconfigDependency = PkgconfigDependency
  { unwrap :: PkgconfigDependency.PkgconfigDependency
  }
  deriving (Eq, Show)

instance Ord PkgconfigDependency where
  compare =
    Ord.comparing $
      (\(PkgconfigDependency.PkgconfigDependency pn pvr) -> (pn, pvr))
        . unwrap

instance Parsec.Parsec PkgconfigDependency where
  parsec = PkgconfigDependency <$> Parsec.parsec

instance Pretty.Pretty PkgconfigDependency where
  pretty = Pretty.pretty . unwrap
