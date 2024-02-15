module CabalGild.Type.Pragma where

import qualified Control.Monad as Monad
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Parsec as Parsec
import qualified System.OsPath as OsPath

newtype Pragma
  = Discover OsPath.OsPath
  deriving (Eq, Show)

instance Parsec.Parsec Pragma where
  parsec = do
    CharParsing.spaces
    Monad.void $ CharParsing.string "cabal-gild:"
    CharParsing.spaces
    Monad.void $ CharParsing.string "discover"
    CharParsing.skipSpaces1
    fp <- Newtypes.getFilePathNT <$> Parsec.parsec
    case OsPath.encodeUtf fp of
      Nothing -> fail $ "invalid file path: " <> show fp
      Just op -> pure $ Discover op
