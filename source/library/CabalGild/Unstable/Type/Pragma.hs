module CabalGild.Unstable.Type.Pragma where

import qualified Control.Monad as Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Parsec as Parsec

-- | A pragma, which is a special comment used to customize behavior.
newtype Pragma
  = -- | Discover modules within the given directory.
    Discover (NonEmpty.NonEmpty FilePath)
  deriving (Eq, Show)

instance Parsec.Parsec Pragma where
  parsec = do
    CharParsing.spaces
    Monad.void $ CharParsing.string "cabal-gild:"
    CharParsing.spaces
    Monad.void $ CharParsing.string "discover"
    CharParsing.skipSpaces1
    Discover
      . fmap Newtypes.getFilePathNT
      <$> CharParsing.sepByNonEmpty Parsec.parsec CharParsing.skipSpaces1
