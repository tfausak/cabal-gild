module CabalGild.Unstable.Type.TestedWith where

import qualified CabalGild.Unstable.Type.VersionRange as VersionRange
import qualified Data.Ord as Ord
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty

-- | This type exists to provide an 'Ord' instance for 'Newtypes.TestedWith',
-- which was added in @Cabal-syntax-3.10.1.0@.
newtype TestedWith = TestedWith
  { unwrap :: Newtypes.TestedWith
  }

instance Eq TestedWith where
  x == y = compare x y == EQ

instance Ord TestedWith where
  compare =
    Ord.comparing $
      fmap VersionRange.fromVersionRange
        . Newtypes.getTestedWith
        . unwrap

instance Parsec.Parsec TestedWith where
  parsec = TestedWith <$> Parsec.parsec

instance Pretty.Pretty TestedWith where
  pretty = Pretty.pretty . unwrap

instance Show TestedWith where
  show = show . Newtypes.getTestedWith . unwrap
