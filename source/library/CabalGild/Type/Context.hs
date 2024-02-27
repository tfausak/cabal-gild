module CabalGild.Type.Context where

import qualified CabalGild.Class.MonadLog as MonadLog
import qualified CabalGild.Type.Config as Config
import qualified CabalGild.Type.Flag as Flag
import qualified CabalGild.Type.Input as Input
import qualified CabalGild.Type.Mode as Mode
import qualified CabalGild.Type.Optional as Optional
import qualified CabalGild.Type.Output as Output
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Version as Version
import qualified Paths_cabal_gild as This
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit

data Context = Context
  { input :: Input.Input,
    mode :: Mode.Mode,
    output :: Output.Output,
    stdin :: FilePath
  }
  deriving (Eq, Show)

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

  pure
    Context
      { input = Optional.withDefault Input.Stdin $ Config.input config,
        mode = Optional.withDefault Mode.Format $ Config.mode config,
        output = Optional.withDefault Output.Stdout $ Config.output config,
        stdin = Optional.withDefault "." $ Config.stdin config
      }
