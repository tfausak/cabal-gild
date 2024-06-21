{-# LANGUAGE FlexibleContexts #-}

-- | This module defines the 'Config' data type.
module CabalGild.Unstable.Type.Config where

import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified CabalGild.Unstable.Type.Input as Input
import qualified CabalGild.Unstable.Type.Leniency as Leniency
import qualified CabalGild.Unstable.Type.Mode as Mode
import qualified CabalGild.Unstable.Type.Optional as Optional
import qualified CabalGild.Unstable.Type.Output as Output
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq

-- | This data type represents the configuration for the command line utility.
-- Each field typically corresponds to a flag.
data Config = Config
  { crlf :: Optional.Optional Leniency.Leniency,
    help :: Optional.Optional Bool,
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
    { crlf = Optional.Default,
      help = Optional.Default,
      input = Optional.Default,
      mode = Optional.Default,
      output = Optional.Default,
      stdin = Optional.Default,
      version = Optional.Default
    }

-- | Applies a flag to the config, returning the new config.
applyFlag ::
  (Exception.MonadThrow m, Writer.MonadWriter (Seq.Seq String) m) =>
  Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.CRLF s -> do
    l <- Leniency.fromString s
    Foldable.for_ (Optional.toMaybe $ crlf config) $ \old ->
      Writer.tell
        . Seq.singleton
        $ unwords ["changing --crlf from", show old, "to", show l]
    pure config {crlf = Optional.Specific l}
  Flag.Help b -> pure config {help = Optional.Specific b}
  Flag.Input s -> do
    Foldable.for_ (Optional.toMaybe $ input config) $ \old ->
      Writer.tell
        . Seq.singleton
        $ unwords ["changing --input from", show old, "to", show s]
    pure config {input = Optional.Specific $ Input.fromString s}
  Flag.IO s -> do
    Foldable.for_ (Optional.toMaybe $ input config) $ \old ->
      Writer.tell
        . Seq.singleton
        $ unwords ["changing --input from", show old, "to", show s]
    Foldable.for_ (Optional.toMaybe $ output config) $ \old ->
      Writer.tell
        . Seq.singleton
        $ unwords ["changing --output from", show old, "to", show s]
    pure
      config
        { input = Optional.Specific $ Input.fromString s,
          output = Optional.Specific $ Output.fromString s
        }
  Flag.Mode s -> do
    Foldable.for_ (Optional.toMaybe $ mode config) $ \old ->
      Writer.tell
        . Seq.singleton
        $ unwords ["changing --mode from", show old, "to", show s]
    m <- Mode.fromString s
    pure config {mode = Optional.Specific m}
  Flag.Output s -> do
    Foldable.for_ (Optional.toMaybe $ output config) $ \old ->
      Writer.tell
        . Seq.singleton
        $ unwords ["changing --output from", show old, "to", show s]
    pure config {output = Optional.Specific $ Output.fromString s}
  Flag.Stdin s -> do
    Foldable.for_ (Optional.toMaybe $ stdin config) $ \old ->
      Writer.tell
        . Seq.singleton
        $ unwords ["changing --stdin from", show old, "to", show s]
    pure config {stdin = Optional.Specific s}
  Flag.Version b -> pure config {version = Optional.Specific b}

-- | Converts a list of flags into a config by starting with 'initial' and
-- repeatedly calling 'applyFlag'.
fromFlags ::
  (Exception.MonadThrow m, Writer.MonadWriter (Seq.Seq String) m) =>
  [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial
