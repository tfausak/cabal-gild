module CabalGild.Type.ExeDependency where

import qualified CabalGild.Type.VersionRange as VersionRange
import qualified Data.Function as Function
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
    Function.on compare $
      (\(ExeDependency.ExeDependency pn ucn vr) -> (pn, ucn, VersionRange.fromVersionRange vr))
        . unwrap

instance Parsec.Parsec ExeDependency where
  parsec = ExeDependency <$> Parsec.parsec

instance Pretty.Pretty ExeDependency where
  pretty = Pretty.pretty . unwrap
