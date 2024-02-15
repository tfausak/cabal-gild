module CabalGild.Type.Pragma where

import qualified Control.Monad as Monad
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Parsec as Parsec

newtype Pragma
  = Discover FilePath
  deriving (Eq, Show)

instance Parsec.Parsec Pragma where
  parsec = do
    CharParsing.spaces
    Monad.void $ CharParsing.string "cabal-gild:"
    CharParsing.spaces
    Monad.void $ CharParsing.string "discover"
    CharParsing.skipSpaces1
    Discover . Newtypes.getFilePathNT <$> Parsec.parsec
