module CabalGild.Type.Config where

import qualified CabalGild.Extra.Either as Either
import qualified CabalGild.Type.Flag as Flag
import qualified CabalGild.Type.Mode as Mode
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified System.OsPath as OsPath

data Config = Config
  { help :: Bool,
    input :: Maybe OsPath.OsPath,
    mode :: Mode.Mode,
    output :: Maybe OsPath.OsPath,
    stdin :: OsPath.OsPath,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { help = False,
      input = Nothing,
      mode = Mode.Format,
      output = Nothing,
      stdin = Either.unsafeFromRight $ OsPath.encodeUtf ".",
      version = False
    }

applyFlag :: (Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help b -> pure config {help = b}
  Flag.Input s -> case s of
    "-" -> pure config {input = Nothing}
    _ -> do
      p <- OsPath.encodeUtf s
      pure config {input = Just p}
  Flag.Mode s -> do
    m <- Mode.fromString s
    pure config {mode = m}
  Flag.Output s -> case s of
    "-" -> pure config {output = Nothing}
    _ -> do
      p <- OsPath.encodeUtf s
      pure config {output = Just p}
  Flag.Stdin s -> do
    p <- OsPath.encodeUtf s
    pure config {stdin = p}
  Flag.Version b -> pure config {version = b}

fromFlags :: (Exception.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial
