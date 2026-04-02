{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Type.Context where

import Bluefin.Eff (Eff, (:>))
import qualified Bluefin.Exception as Exception
import qualified CabalGild.Unstable.Effect.Handle as Handle
import qualified CabalGild.Unstable.Effect.Log as Log
import qualified CabalGild.Unstable.Effect.Walk as Walk
import qualified CabalGild.Unstable.Effect.Warn as Warn
import qualified CabalGild.Unstable.Exception.MixedArgumentStyles as MixedArgumentStyles
import qualified CabalGild.Unstable.Exception.MoreThanOneCabalFileFound as MoreThanOneCabalFileFound
import qualified CabalGild.Unstable.Exception.NoCabalFileFound as NoCabalFileFound
import qualified CabalGild.Unstable.Exception.SpecifiedOutputWithCheckMode as SpecifiedOutputWithCheckMode
import qualified CabalGild.Unstable.Exception.SpecifiedStdinWithFileInput as SpecifiedStdinWithFileInput
import qualified CabalGild.Unstable.Type.Config as Config
import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified CabalGild.Unstable.Type.Input as Input
import qualified CabalGild.Unstable.Type.Leniency as Leniency
import qualified CabalGild.Unstable.Type.Mode as Mode
import qualified CabalGild.Unstable.Type.Optional as Optional
import qualified CabalGild.Unstable.Type.Output as Output
import qualified Control.Exception as E
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Version as Version
import qualified Paths_cabal_gild as This
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | Represents the context necessary to run the program. This is essentially a
-- simplified 'Config.Config'.
data Context = Context
  { crlf :: Leniency.Leniency,
    input :: Input.Input,
    mode :: Mode.Mode,
    output :: Output.Output,
    stdin :: FilePath
  }
  deriving (Eq, Show)

-- | Creates a 'Context' from a 'Config.Config'. If the help or version was
-- requested, then this will throw an 'Exit.ExitSuccess'. Otherwise this makes
-- sure the config is valid before returning the context.
fromConfig ::
  (eH :> es, eL :> es, eWn :> es, eX :> es, eWk :> es) =>
  Handle.Handle eH ->
  Log.Log eL ->
  Warn.Warn eWn ->
  Exception.Exception E.SomeException eX ->
  Walk.Walk eWk ->
  Config.Config ->
  Eff es Context
fromConfig handleH logH warnH ex walkH config = do
  let version = Version.showVersion This.version

  Monad.when (Maybe.fromMaybe False . Optional.toMaybe $ Config.help config) $ do
    let header =
          unlines
            [ "cabal-gild version " <> version,
              "",
              "<https://github.com/tfausak/cabal-gild>",
              "",
              "Usage: cabal-gild [OPTIONS] [FILE ...]"
            ]
    Log.logLn logH
      . List.dropWhileEnd Char.isSpace
      $ GetOpt.usageInfo header Flag.options
    Exception.throw ex $ E.toException Exit.ExitSuccess

  Monad.when (Maybe.fromMaybe False . Optional.toMaybe $ Config.version config) $ do
    Log.logLn logH version
    Exception.throw ex $ E.toException Exit.ExitSuccess

  case (Config.files config, Config.input config, Config.output config) of
    (_ : _, Optional.Specific _, _) ->
      Exception.throw ex . E.toException $ MixedArgumentStyles.MixedArgumentStyles Flag.inputOption
    (_ : _, _, Optional.Specific _) ->
      Exception.throw ex . E.toException $ MixedArgumentStyles.MixedArgumentStyles Flag.outputOption
    ([], Optional.Specific _, Optional.Specific _) -> do
      Warn.warnLn warnH "warning: --input is deprecated, use a positional argument instead"
      Warn.warnLn warnH "warning: --output is deprecated, use piping instead"
    ([], Optional.Specific _, _) ->
      Warn.warnLn warnH "warning: --input is deprecated, use a positional argument instead"
    ([], _, Optional.Specific _) ->
      Warn.warnLn warnH "warning: --output is deprecated, use piping instead"
    _ -> pure ()

  case (Config.files config, Config.stdin config) of
    (_ : _, Optional.Specific _) ->
      Exception.throw ex . E.toException $ MixedArgumentStyles.MixedArgumentStyles Flag.stdinOption
    _ -> pure ()

  case (Config.input config, Config.stdin config) of
    (Optional.Specific (Input.File _), Optional.Specific _) ->
      Exception.throw ex $ E.toException SpecifiedStdinWithFileInput.SpecifiedStdinWithFileInput
    _ -> pure ()

  case (Config.mode config, Config.output config) of
    (Optional.Specific Mode.Check, Optional.Specific _) ->
      Exception.throw ex $ E.toException SpecifiedOutputWithCheckMode.SpecifiedOutputWithCheckMode
    _ -> pure ()

  let preInput = Maybe.fromMaybe Input.Stdin . Optional.toMaybe $ Config.input config
      filePath = case preInput of
        Input.Stdin -> "."
        Input.File f -> f
      preOutput = Maybe.fromMaybe Output.Stdout . Optional.toMaybe $ Config.output config
  (theInput, theOutput) <- do
    isTerm <- Handle.isTerminalDevice handleH IO.stdin
    if null (Config.files config) && preInput == Input.Stdin && preOutput == Output.Stdout && isTerm
      then do
        cabalFiles <- Walk.walk walkH "." ["*.cabal"] []
        case cabalFiles of
          [fp] -> pure (Input.File fp, Output.File fp)
          [] -> Exception.throw ex $ E.toException NoCabalFileFound.NoCabalFileFound
          _ -> Exception.throw ex $ E.toException MoreThanOneCabalFileFound.MoreThanOneCabalFileFound
      else pure (preInput, preOutput)
  pure
    Context
      { crlf = Maybe.fromMaybe Leniency.Lenient . Optional.toMaybe $ Config.crlf config,
        input = theInput,
        mode = Maybe.fromMaybe Mode.Format . Optional.toMaybe $ Config.mode config,
        output = theOutput,
        stdin = Maybe.fromMaybe filePath . Optional.toMaybe $ Config.stdin config
      }
