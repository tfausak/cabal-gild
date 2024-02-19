module CabalGild.Type.TestedWith where

import qualified CabalGild.Type.VersionRange as VersionRange
import qualified Data.Function as Function
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty

-- | This type exists to provide an 'Ord' instance for 'Newtypes.TestedWith',
-- which was added in @Cabal-syntax-3.10.1.0@.
newtype TestedWith = TestedWith
  { unwrap :: Newtypes.TestedWith
  }

instance Eq TestedWith where
  (==) =
    Function.on (==) $
      fmap VersionRange.fromVersionRange
        . Newtypes.getTestedWith
        . unwrap

instance Ord TestedWith where
  compare =
    Function.on compare $
      fmap VersionRange.fromVersionRange
        . Newtypes.getTestedWith
        . unwrap

instance Parsec.Parsec TestedWith where
  parsec = TestedWith <$> Parsec.parsec

instance Pretty.Pretty TestedWith where
  pretty = Pretty.pretty . unwrap

instance Show TestedWith where
  show = show . Newtypes.getTestedWith . unwrap
