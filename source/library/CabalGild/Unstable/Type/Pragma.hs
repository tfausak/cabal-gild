module CabalGild.Unstable.Type.Pragma where

import qualified Control.Monad as Monad
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Parsec as Parsec

-- | A pragma, which is a special comment used to customize behavior.
newtype Pragma
  = -- | Discover modules using the given arguments.
    Discover [String]
  deriving (Eq, Show)

instance Parsec.Parsec Pragma where
  parsec = do
    CharParsing.spaces
    Monad.void $ CharParsing.string "cabal-gild:"
    CharParsing.spaces
    Monad.void $ CharParsing.string "discover"
    arguments <-
      Monad.mplus
        (CharParsing.skipSpaces1 *> CharParsing.sepBy Parsec.parsec CharParsing.skipSpaces1)
        ([] <$ CharParsing.spaces)
    CharParsing.eof
    pure . Discover $ fmap Newtypes.getToken' arguments
