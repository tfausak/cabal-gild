-- | This module defines the main entry point for the application.
module CabalGild.Main where

import qualified CabalGild.Action.AttachComments as AttachComments
import qualified CabalGild.Action.EvaluatePragmas as EvaluatePragmas
import qualified CabalGild.Action.ExtractComments as ExtractComments
import qualified CabalGild.Action.FormatFields as FormatFields
import qualified CabalGild.Action.GetCabalVersion as GetCabalVersion
import qualified CabalGild.Action.ReflowText as ReflowText
import qualified CabalGild.Action.RemovePositions as RemovePositions
import qualified CabalGild.Action.Render as Render
import qualified CabalGild.Action.StripBlanks as StripBlanks
import qualified CabalGild.Class.MonadLog as MonadLog
import qualified CabalGild.Class.MonadRead as MonadRead
import qualified CabalGild.Class.MonadWalk as MonadWalk
import qualified CabalGild.Class.MonadWrite as MonadWrite
import qualified CabalGild.Exception.CheckFailure as CheckFailure
import qualified CabalGild.Exception.ParseError as ParseError
import qualified CabalGild.Extra.ByteString as ByteString
import qualified CabalGild.Type.Config as Config
import qualified CabalGild.Type.Context as Context
import qualified CabalGild.Type.Flag as Flag
import qualified CabalGild.Type.Leniency as Leniency
import qualified CabalGild.Type.Mode as Mode
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Distribution.Fields as Fields
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | This is the main entry point for the application. It gets the command line
-- arguments and then hands things off to 'mainWith'. If any exceptions are
-- thrown, they will be handled by 'onException'.
defaultMain :: IO ()
defaultMain = Exception.handle onException $ do
  arguments <- Environment.getArgs
  mainWith arguments

-- | If the exception was an 'Exit.ExitCode', simply exit with that code.
-- Otherwise handle exceptions by printing them to STDERR using
-- 'Exception.displayException' instead of 'show'. Then exit with a failing
-- status code.
onException :: Exception.SomeException -> IO a
onException e = case Exception.fromException e of
  Just exitCode -> Exit.exitWith exitCode
  Nothing -> do
    IO.hPutStrLn IO.stderr $ Exception.displayException e
    Exit.exitFailure

-- | The actual logic for the command line application. This is written using
-- constraints so that it can be run in pure code if so desired. But most often
-- this will be run in 'IO'.
mainWith ::
  ( MonadLog.MonadLog m,
    MonadRead.MonadRead m,
    Exception.MonadThrow m,
    MonadWalk.MonadWalk m,
    MonadWrite.MonadWrite m
  ) =>
  [String] ->
  m ()
mainWith arguments = do
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig config

  input <- MonadRead.read $ Context.input context
  output <- format (Context.stdin context) input

  case Context.mode context of
    Mode.Check -> do
      let formatted = case Context.crlf context of
            Leniency.Lenient ->
              let lf = ByteString.singleton 0x0a
                  crlf = ByteString.cons 0x0d lf
               in output == ByteString.replace crlf lf input
            Leniency.Strict -> output == input
      Monad.unless formatted $ Exception.throwM CheckFailure.CheckFailure
    Mode.Format -> MonadWrite.write (Context.output context) output

-- | Formats the given input using the provided file path as the apparent
-- source file (see 'Context.stdin'). An exception will be thrown if the input
-- is invalid. The 'MonadWalk.MonadWalk' constraint is used to discover modules
-- on the file system. Typically @m@ will be 'IO'.
format ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  FilePath ->
  ByteString.ByteString ->
  m ByteString.ByteString
format filePath input = do
  fields <-
    either (Exception.throwM . ParseError.ParseError) pure $
      Fields.readFields input
  let csv = GetCabalVersion.fromFields fields
      comments = ExtractComments.fromByteString input
  ( StripBlanks.run
      Monad.>=> AttachComments.run
      Monad.>=> ReflowText.run csv
      Monad.>=> RemovePositions.run
      Monad.>=> EvaluatePragmas.run filePath
      Monad.>=> FormatFields.run csv
      Monad.>=> Render.run
    )
    (fields, comments)
