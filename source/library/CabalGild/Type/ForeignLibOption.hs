module CabalGild.Type.ForeignLibOption where

import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.ForeignLibOption as ForeignLibOption

-- | This type exists to provide an 'Ord' instance for
-- 'ForeignLibOption.ForeignLibOption', which was added in
-- @Cabal-syntax-3.10.1.0@.
newtype ForeignLibOption = ForeignLibOption
  { unwrap :: ForeignLibOption.ForeignLibOption
  }
  deriving (Eq, Show)

instance Ord ForeignLibOption where
  compare = Ord.comparing Pretty.prettyShow

instance Parsec.Parsec ForeignLibOption where
  parsec = ForeignLibOption <$> Parsec.parsec

instance Pretty.Pretty ForeignLibOption where
  pretty = Pretty.pretty . unwrap
