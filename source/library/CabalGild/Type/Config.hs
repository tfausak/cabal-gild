module CabalGild.Type.Config where

import qualified CabalGild.Options as Options
import qualified CabalGild.Type.Flag as Flag
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Text.Read as Read

data Config = Config
  { help :: Bool,
    options :: Options.Options,
    version :: Bool
  }
  deriving (Show)

initial :: Config
initial =
  Config
    { help = False,
      options = Options.defaultOptions,
      version = False
    }

applyFlag :: (Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag c f = case f of
  Flag.CabalFile b -> pure c {options = (options c) {Options.optCabalFile = b}}
  Flag.Error b -> pure c {options = (options c) {Options.optError = b}}
  Flag.Help -> pure c {help = True}
  Flag.Indent s -> do
    i <- case Read.readMaybe s of
      Nothing -> Exception.throwM . userError $ "invalid indent: " <> show s
      Just i -> pure i
    pure c {options = (options c) {Options.optIndent = i}}
  Flag.Mode m -> pure c {options = (options c) {Options.optMode = m}}
  Flag.StdinInputFile s -> pure c {options = (options c) {Options.optStdinInputFile = Just s}}
  Flag.Tabular b -> pure c {options = (options c) {Options.optTabular = b}}
  Flag.Version -> pure c {version = True}

fromFlags :: (Exception.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial
