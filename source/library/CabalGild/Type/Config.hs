-- | This module defines the 'Config' data type.
module CabalGild.Type.Config where

import qualified CabalGild.Type.Flag as Flag
import qualified CabalGild.Type.Input as Input
import qualified CabalGild.Type.Mode as Mode
import qualified CabalGild.Type.Optional as Optional
import qualified CabalGild.Type.Output as Output
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception

-- | This data type represents the configuration for the command line utility.
-- Each field typically corresponds to a flag.
data Config = Config
  { help :: Optional.Optional Bool,
    input :: Optional.Optional Input.Input,
    mode :: Optional.Optional Mode.Mode,
    output :: Optional.Optional Output.Output,
    stdin :: Optional.Optional FilePath,
    version :: Optional.Optional Bool
  }
  deriving (Eq, Show)

-- | The default config.
initial :: Config
initial =
  Config
    { help = Optional.Default,
      input = Optional.Default,
      mode = Optional.Default,
      output = Optional.Default,
      stdin = Optional.Default,
      version = Optional.Default
    }

-- | Applies a flag to the config, returning the new config.
applyFlag :: (Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help b -> pure config {help = Optional.Specific b}
  Flag.Input s -> pure config {input = Optional.Specific $ Input.fromString s}
  Flag.IO s ->
    pure
      config
        { input = Optional.Specific $ Input.fromString s,
          output = Optional.Specific $ Output.fromString s
        }
  Flag.Mode s -> do
    m <- Mode.fromString s
    pure config {mode = Optional.Specific m}
  Flag.Output s -> pure config {output = Optional.Specific $ Output.fromString s}
  Flag.Stdin s -> pure config {stdin = Optional.Specific s}
  Flag.Version b -> pure config {version = Optional.Specific b}

-- | Converts a list of flags into a config by starting with 'initial' and
-- repeatedly calling 'applyFlag'.
fromFlags :: (Exception.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial
