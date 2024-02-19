module CabalGild.Type.Dependency where

import qualified CabalGild.Type.VersionRange as VersionRange
import qualified Data.Function as Function
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
    Function.on compare $
      (\(Dependency.Dependency pn vr lns) -> (pn, VersionRange.fromVersionRange vr, lns))
        . unwrap

instance Parsec.Parsec Dependency where
  parsec = Dependency <$> Parsec.parsec

instance Pretty.Pretty Dependency where
  pretty = Pretty.pretty . unwrap
