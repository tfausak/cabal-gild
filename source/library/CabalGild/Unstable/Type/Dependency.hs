module CabalGild.Unstable.Type.Dependency where

import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.Dependency as Dependency

-- | This type exists to provide an 'Ord' instance for
-- 'Dependency.Dependency', which was added in @Cabal-syntax-3.10.1.0@.
newtype Dependency = Dependency
  { unwrap :: Dependency.Dependency
  }
  deriving (Eq, Show)

instance Ord Dependency where
  compare =
    Ord.comparing $
      (\(Dependency.Dependency pn vr lns) -> (pn, vr, lns))
        . unwrap

instance Parsec.Parsec Dependency where
  parsec = Dependency <$> Parsec.parsec

instance Pretty.Pretty Dependency where
  pretty = Pretty.pretty . unwrap
