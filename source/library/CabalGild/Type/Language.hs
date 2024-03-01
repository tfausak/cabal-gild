module CabalGild.Type.Language where

import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Language.Haskell.Extension as Extension

-- | This type exists to provide an 'Ord' instance for
-- 'Language.Language', which was added in @Cabal-syntax-3.10.1.0@.
newtype Language = Language
  { unwrap :: Extension.Language
  }
  deriving (Eq, Show)

instance Ord Language where
  compare = Ord.comparing Pretty.prettyShow

instance Parsec.Parsec Language where
  parsec = Language <$> Parsec.parsec

instance Pretty.Pretty Language where
  pretty = Pretty.pretty . unwrap
