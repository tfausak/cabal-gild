{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines the main entry point for the application.
module CabalGild.Unstable.Main where

import Bluefin.Eff (Eff, (:>))
import qualified Bluefin.Eff as Eff
import qualified Bluefin.Exception as Exception
import qualified Bluefin.IO as BIO
import qualified CabalGild.Unstable.Action.AttachComments as AttachComments
import qualified CabalGild.Unstable.Action.EvaluatePragmas as EvaluatePragmas
import qualified CabalGild.Unstable.Action.ExtractComments as ExtractComments
import qualified CabalGild.Unstable.Action.FormatFields as FormatFields
import qualified CabalGild.Unstable.Action.GetCabalVersion as GetCabalVersion
import qualified CabalGild.Unstable.Action.ReflowText as ReflowText
import qualified CabalGild.Unstable.Action.Render as Render
import qualified CabalGild.Unstable.Action.StripBlanks as StripBlanks
import qualified CabalGild.Unstable.Effect.Handle as Handle
import qualified CabalGild.Unstable.Effect.Log as Log
import qualified CabalGild.Unstable.Effect.Read as Read
import qualified CabalGild.Unstable.Effect.Walk as Walk
import qualified CabalGild.Unstable.Effect.Warn as Warn
import qualified CabalGild.Unstable.Effect.Write as Write
import qualified CabalGild.Unstable.Exception.CheckFailure as CheckFailure
import qualified CabalGild.Unstable.Exception.ParseError as ParseError
import qualified CabalGild.Unstable.Extra.ByteString as ByteString
import qualified CabalGild.Unstable.Type.Config as Config
import qualified CabalGild.Unstable.Type.Context as Context
import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified CabalGild.Unstable.Type.Input as Input
import qualified CabalGild.Unstable.Type.Leniency as Leniency
import qualified CabalGild.Unstable.Type.Mode as Mode
import qualified CabalGild.Unstable.Type.Output as Output
import qualified Control.Exception as E
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Distribution.Fields as Fields
import Prelude hiding (Read, read)
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | This is the main entry point for the application. It gets the command line
-- arguments and then hands things off to 'mainWith'. If any exceptions are
-- thrown, they will be handled by 'onException'.
defaultMain :: IO ()
defaultMain = E.handle onException $ Eff.runEff $ \ioe -> do
  arguments <- BIO.effIO ioe Environment.getArgs
  let handleH = Handle.makeHandleIO ioe
      logH = Log.makeLogIO ioe
      readH = Read.makeReadIO ioe
      walkH = Walk.makeWalkIO ioe
      warnH = Warn.makeWarnIO ioe
      writeH = Write.makeWriteIO ioe
  result <- Exception.try $ \ex ->
    mainWith handleH logH readH ex walkH warnH writeH arguments
  case result of
    Right () -> pure ()
    Left e -> BIO.effIO ioe $ E.throwIO e

-- | If the exception was an 'Exit.ExitCode', simply exit with that code.
-- Otherwise handle exceptions by printing them to STDERR using
-- 'E.displayException' instead of 'show'. Then exit with a failing
-- status code.
onException :: E.SomeException -> IO a
onException e = case E.fromException e of
  Just exitCode -> Exit.exitWith exitCode
  Nothing -> do
    IO.hPutStrLn IO.stderr $ E.displayException e
    Exit.exitFailure

-- | The actual logic for the command line application. This is written using
-- bluefin effects so that it can be run in pure code if so desired. But most
-- often this will be run in 'IO'.
mainWith ::
  (eH :> es, eL :> es, eR :> es, eX :> es, eWk :> es, eWn :> es, eW :> es) =>
  Handle.Handle eH ->
  Log.Log eL ->
  Read.Read eR ->
  Exception.Exception E.SomeException eX ->
  Walk.Walk eWk ->
  Warn.Warn eWn ->
  Write.Write eW ->
  [String] ->
  Eff es ()
mainWith handleH logH readH ex walkH warnH writeH arguments = do
  (flags, positionalArgs) <- Flag.fromArguments ex arguments
  config <- Config.fromFlags ex flags positionalArgs
  context <- Context.fromConfig handleH logH warnH ex walkH config

  case Config.files config of
    [] -> processOne readH ex walkH warnH writeH context
    fps -> processFiles readH ex walkH warnH writeH context fps

processFiles ::
  forall eR eX eWk eWn eW es.
  (eR :> es, eX :> es, eWk :> es, eWn :> es, eW :> es) =>
  Read.Read eR ->
  Exception.Exception E.SomeException eX ->
  Walk.Walk eWk ->
  Warn.Warn eWn ->
  Write.Write eW ->
  Context.Context ->
  [FilePath] ->
  Eff es ()
processFiles readH ex walkH warnH writeH context = go False
  where
    go :: Bool -> [FilePath] -> Eff es ()
    go anyFail [] =
      Monad.when anyFail $ Exception.throw ex (E.toException CheckFailure.CheckFailure)
    go anyFail (fp : fps) = do
      formatted <- processFileInner readH ex walkH warnH writeH context fp
      go (anyFail || not formatted) fps

processFileInner ::
  (eR :> es, eX :> es, eWk :> es, eWn :> es, eW :> es) =>
  Read.Read eR ->
  Exception.Exception E.SomeException eX ->
  Walk.Walk eWk ->
  Warn.Warn eWn ->
  Write.Write eW ->
  Context.Context ->
  FilePath ->
  Eff es Bool
processFileInner readH ex walkH warnH writeH context fp =
  let ctx =
        context
          { Context.input = Input.File fp,
            Context.output = Output.File fp,
            Context.stdin = fp
          }
   in processOneInner readH ex walkH warnH writeH ctx

processOne ::
  (eR :> es, eX :> es, eWk :> es, eWn :> es, eW :> es) =>
  Read.Read eR ->
  Exception.Exception E.SomeException eX ->
  Walk.Walk eWk ->
  Warn.Warn eWn ->
  Write.Write eW ->
  Context.Context ->
  Eff es ()
processOne readH ex walkH warnH writeH context = do
  formatted <- processOneInner readH ex walkH warnH writeH context
  Monad.unless formatted $ Exception.throw ex (E.toException CheckFailure.CheckFailure)

-- | Process a single file. Returns True if the file is already formatted.
processOneInner ::
  (eR :> es, eX :> es, eWk :> es, eWn :> es, eW :> es) =>
  Read.Read eR ->
  Exception.Exception E.SomeException eX ->
  Walk.Walk eWk ->
  Warn.Warn eWn ->
  Write.Write eW ->
  Context.Context ->
  Eff es Bool
processOneInner readH ex walkH warnH writeH context = do
  input <- Read.read readH $ Context.input context
  output <- format ex readH walkH warnH (Context.stdin context) input

  let formatted = check (Context.crlf context) input output
  case Context.mode context of
    Mode.Check -> pure formatted
    Mode.Format -> do
      case (Context.input context, Context.output context) of
        (Input.File i, Output.File o) | formatted, i == o -> pure ()
        (_, o) -> Write.write writeH o output
      pure True

-- | Formats the given input using the provided file path as the apparent
-- source file (see 'Context.stdin'). An exception will be thrown if the input
-- is invalid.
format ::
  (eX :> es, eR :> es, eWk :> es, eWn :> es) =>
  Exception.Exception E.SomeException eX ->
  Read.Read eR ->
  Walk.Walk eWk ->
  Warn.Warn eWn ->
  FilePath ->
  ByteString.ByteString ->
  Eff es ByteString.ByteString
format ex readH walkH warnH filePath input = do
  fields <-
    either (Exception.throw ex . E.toException . ParseError.ParseError) pure $
      Fields.readFields input
  let csv = GetCabalVersion.fromFields fields
      comments = ExtractComments.fromByteString input
  StripBlanks.run (fields, comments)
    >>= AttachComments.run
    >>= ReflowText.run csv
    >>= EvaluatePragmas.run ex readH walkH warnH filePath
    >>= FormatFields.run csv
    >>= Render.run csv

-- | Returns true if the output is formatted correctly, false otherwise.
check ::
  Leniency.Leniency ->
  ByteString.ByteString ->
  ByteString.ByteString ->
  Bool
check leniency input output = case leniency of
  Leniency.Lenient ->
    let lf = ByteString.singleton 0x0a
        crlf = ByteString.cons 0x0d lf
     in output == ByteString.replace crlf lf input
  Leniency.Strict -> output == input
