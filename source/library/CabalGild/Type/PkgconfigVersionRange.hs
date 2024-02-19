module CabalGild.Type.PkgconfigVersionRange where

import qualified CabalGild.Type.VersionRange as VersionRange
import qualified Data.Function as Function
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.PkgconfigVersionRange as PkgconfigVersionRange

-- | This type exists to provide an 'Ord' instance for
-- 'PkgconfigVersionRange.PkgconfigVersionRange', which was added in
-- @Cabal-syntax-3.10.1.0@.
newtype PkgconfigVersionRange = PkgconfigVersionRange
  { unwrap :: PkgconfigVersionRange.PkgconfigVersionRange
  }
  deriving (Eq, Show)

instance Ord PkgconfigVersionRange where
  compare =
    Function.on compare $
      VersionRange.fromPkgconfigVersionRange
        . unwrap

instance Parsec.Parsec PkgconfigVersionRange where
  parsec = PkgconfigVersionRange <$> Parsec.parsec

instance Pretty.Pretty PkgconfigVersionRange where
  pretty = Pretty.pretty . unwrap
