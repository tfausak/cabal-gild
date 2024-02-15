module CabalGild.Main where

import qualified CabalGild.Action.AttachComments as AttachComments
import qualified CabalGild.Action.Discover as Discover
import qualified CabalGild.Action.ExtractComments as ExtractComments
import qualified CabalGild.Action.Format as Format
import qualified CabalGild.Action.GetCabalVersion as GetCabalVersion
import qualified CabalGild.Action.Reindent as Reindent
import qualified CabalGild.Action.RemovePositions as RemovePositions
import qualified CabalGild.Action.Render as Render
import qualified CabalGild.Class.MonadLog as MonadLog
import qualified CabalGild.Class.MonadRead as MonadRead
import qualified CabalGild.Class.MonadWalk as MonadWalk
import qualified CabalGild.Class.MonadWrite as MonadWrite
import qualified CabalGild.Exception.CheckFailure as CheckFailure
import qualified CabalGild.Type.Config as Config
import qualified CabalGild.Type.Flag as Flag
import qualified CabalGild.Type.Mode as Mode
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe
import qualified Data.Version as Version
import qualified Distribution.Fields as Fields
import qualified Paths_cabal_gild as This
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = Exception.handle onException $ do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

onException :: Exception.SomeException -> IO a
onException e = case Exception.fromException e of
  Just exitCode -> Exit.exitWith exitCode
  Nothing -> do
    IO.hPutStrLn IO.stderr $ Exception.displayException e
    Exit.exitFailure

mainWith ::
  ( MonadLog.MonadLog m,
    MonadRead.MonadRead m,
    Exception.MonadThrow m,
    MonadWalk.MonadWalk m,
    MonadWrite.MonadWrite m
  ) =>
  String ->
  [String] ->
  m ()
mainWith name arguments = do
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  let version = Version.showVersion This.version

  Monad.when (Config.help config) $ do
    let header =
          unlines
            [ name <> " version " <> version,
              "",
              "<https://github.com/tfausak/cabal-gild>"
            ]
    MonadLog.log $ GetOpt.usageInfo header Flag.options
    Exception.throwM Exit.ExitSuccess

  Monad.when (Config.version config) $ do
    MonadLog.logLn version
    Exception.throwM Exit.ExitSuccess

  input <- MonadRead.read $ Config.input config
  fields <- either Exception.throwM pure $ Fields.readFields input
  let csv = GetCabalVersion.fromFields fields
      comments = ExtractComments.fromByteString input
  output <-
    ( AttachComments.run
        Monad.>=> Reindent.run csv
        Monad.>=> RemovePositions.run
        Monad.>=> Discover.run (Maybe.fromMaybe (Config.stdin config) $ Config.input config)
        Monad.>=> Format.run csv
        Monad.>=> Render.run
      )
      (fields, comments)

  case Config.mode config of
    Mode.Check ->
      Monad.when (output /= input) $
        Exception.throwM CheckFailure.CheckFailure
    Mode.Format -> MonadWrite.write (Config.output config) output
