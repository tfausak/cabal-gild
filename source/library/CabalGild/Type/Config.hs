-- | This module defines the 'Config' data type.
module CabalGild.Type.Config where

import qualified CabalGild.Type.Flag as Flag
import qualified CabalGild.Type.Mode as Mode
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception

-- | This data type represents the configuration for the command line utility.
-- Each field typically corresponds to a flag.
data Config = Config
  { help :: Bool,
    input :: Maybe FilePath,
    mode :: Mode.Mode,
    output :: Maybe FilePath,
    stdin :: FilePath,
    version :: Bool
  }
  deriving (Eq, Show)

-- | The default config.
initial :: Config
initial =
  Config
    { help = False,
      input = Nothing,
      mode = Mode.Format,
      output = Nothing,
      stdin = ".",
      version = False
    }

-- | Applies a flag to the config, returning the new config.
applyFlag :: (Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help b -> pure config {help = b}
  Flag.Input s -> case s of
    "-" -> pure config {input = Nothing}
    _ -> pure config {input = Just s}
  Flag.Mode s -> do
    m <- Mode.fromString s
    pure config {mode = m}
  Flag.Output s -> case s of
    "-" -> pure config {output = Nothing}
    _ -> pure config {output = Just s}
  Flag.Stdin s -> pure config {stdin = s}
  Flag.Version b -> pure config {version = b}

-- | Converts a list of flags into a config by starting with 'initial' and
-- repeatedly calling 'applyFlag'.
fromFlags :: (Exception.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial
