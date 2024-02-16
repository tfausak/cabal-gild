module CabalGild.Exception.ParseError where

import qualified Control.Monad.Catch as Exception
import qualified Text.Parsec.Error as Parsec

-- | This type only exists to add an 'Exception.Exception' instance to the
-- 'Parsec.ParseError' type. That instance was added in @parsec-3.1.17.0@. See:
-- <https://github.com/haskell/parsec/pull/178>.
newtype ParseError = ParseError
  { unwrap :: Parsec.ParseError
  }
  deriving (Eq, Show)

instance Exception.Exception ParseError where
  displayException = mappend "parse error: " . show . unwrap
