module CabalGild.Unstable.Type.Context where

import qualified CabalGild.Unstable.Class.MonadLog as MonadLog
import qualified CabalGild.Unstable.Exception.SpecifiedCrlfWithFormatMode as SpecifiedCrlfWithFormatMode
import qualified CabalGild.Unstable.Exception.SpecifiedOutputWithCheckMode as SpecifiedOutputWithCheckMode
import qualified CabalGild.Unstable.Exception.SpecifiedStdinWithFileInput as SpecifiedStdinWithFileInput
import qualified CabalGild.Unstable.Type.Config as Config
import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified CabalGild.Unstable.Type.Input as Input
import qualified CabalGild.Unstable.Type.Leniency as Leniency
import qualified CabalGild.Unstable.Type.Mode as Mode
import qualified CabalGild.Unstable.Type.Optional as Optional
import qualified CabalGild.Unstable.Type.Output as Output
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Version as Version
import qualified Paths_cabal_gild as This
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit

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
  (MonadLog.MonadLog m, Exception.MonadThrow m) =>
  Config.Config ->
  m Context
fromConfig config = do
  let version = Version.showVersion This.version

  Monad.when (Optional.withDefault False $ Config.help config) $ do
    let header =
          unlines
            [ "cabal-gild version " <> version,
              "",
              "<https://github.com/tfausak/cabal-gild>"
            ]
    MonadLog.logLn
      . List.dropWhileEnd Char.isSpace
      $ GetOpt.usageInfo header Flag.options
    Exception.throwM Exit.ExitSuccess

  Monad.when (Optional.withDefault False $ Config.version config) $ do
    MonadLog.logLn version
    Exception.throwM Exit.ExitSuccess

  case (Config.input config, Config.stdin config) of
    (Optional.Specific (Input.File _), Optional.Specific _) ->
      Exception.throwM SpecifiedStdinWithFileInput.SpecifiedStdinWithFileInput
    _ -> pure ()

  case (Config.mode config, Config.output config) of
    (Optional.Specific Mode.Check, Optional.Specific _) ->
      Exception.throwM SpecifiedOutputWithCheckMode.SpecifiedOutputWithCheckMode
    _ -> pure ()

  case Config.crlf config of
    Optional.Specific _
      | Config.mode config /= Optional.Specific Mode.Check ->
          Exception.throwM SpecifiedCrlfWithFormatMode.SpecifiedCrlfWithFormatMode
    _ -> pure ()

  let theInput = Optional.withDefault Input.Stdin $ Config.input config
      filePath = case theInput of
        Input.Stdin -> "."
        Input.File f -> f
  pure
    Context
      { crlf = Optional.withDefault Leniency.Lenient $ Config.crlf config,
        input = theInput,
        mode = Optional.withDefault Mode.Format $ Config.mode config,
        output = Optional.withDefault Output.Stdout $ Config.output config,
        stdin = Optional.withDefault filePath $ Config.stdin config
      }
