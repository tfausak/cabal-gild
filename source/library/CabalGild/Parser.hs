-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Parser where

import CabalGild.Error
import CabalGild.Monad
import qualified Control.Monad.Except as Except
import qualified Data.ByteString as BS
import qualified Distribution.Fields as C
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Parsec as C
import qualified Distribution.Types.GenericPackageDescription as C

runParseResult :: (MonadCabalGild r m) => FilePath -> BS.ByteString -> C.ParseResult a -> m a
runParseResult filepath contents pr = case result of
  Right gpd -> return gpd
  Left (mspecVersion, errors) -> Except.throwError $ CabalParseError filepath contents errors mspecVersion warnings
  where
    (warnings, result) = C.runParseResult pr

parseGpd :: (MonadCabalGild r m) => FilePath -> BS.ByteString -> m C.GenericPackageDescription
parseGpd filepath contents = runParseResult filepath contents $ C.parseGenericPackageDescription contents

parseFields :: (MonadCabalGild r m) => BS.ByteString -> m [C.Field C.Position]
parseFields contents = case C.readFields contents of
  Left err -> Except.throwError $ PanicCannotParseInput err
  Right x -> return x
