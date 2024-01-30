-- |
-- License: BSD-3-Clause
-- Copyright: Oleg Grenrus
module CabalGild.Main (main) where

import CabalGild (cabalGild)
import CabalGild.Error (Error (SomeError), renderError)
import CabalGild.Monad (runCabalGildIO)
import CabalGild.Options
import qualified CabalGild.Type.Config as Config
import qualified CabalGild.Type.Flag as Flag
import qualified CabalGild.Type.Mode as Mode
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import Data.Traversable (for)
import qualified Data.Version as Version
import Distribution.Utils.Generic (toUTF8BS)
import qualified Paths_cabal_gild as This
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import System.Exit (exitFailure)
import qualified System.Exit as Exit
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  arguments <- Environment.getArgs
  let (flags, filepaths, unknowns, invalids) = GetOpt.getOpt' GetOpt.Permute Flag.options arguments
  mapM_ (Exception.throwM . userError . mappend "unknown option: ") unknowns
  mapM_ (Exception.throwM . userError . mappend "invalid option: ") invalids
  config <- Config.fromFlags flags
  let opts = Config.options config

  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo name Flag.options
    Exit.exitSuccess

  Monad.when (Config.version config) $ do
    putStrLn $ Version.showVersion This.version
    Exit.exitSuccess

  notFormatted <-
    Maybe.catMaybes <$> case filepaths of
      [] -> fmap pure $ BS.getContents >>= main' opts Nothing
      (_ : _) -> for filepaths $ \filepath -> do
        contents <- BS.readFile filepath
        main' opts (Just filepath) contents

  Monad.when ((optMode opts == Mode.Check) && not (null notFormatted)) $ do
    Monad.forM_ notFormatted $ \filepath ->
      hPutStrLn stderr $ "error: Input " <> filepath <> " is not formatted."
    exitFailure

main' :: Options -> Maybe FilePath -> BS.ByteString -> IO (Maybe FilePath)
main' opts mfilepath input = do
  -- name of the input
  let filepath = Maybe.fromMaybe "<stdin>" mfilepath

  mroot <-
    fmap takeDirectory <$> case (mfilepath, optStdinInputFile opts) of
      (Just _, Just _) -> do
        renderError $ SomeError "cannot pass both --stdin-input-file and FILE"
        exitFailure
      (Just f, Nothing) -> pure $ Just f
      (Nothing, Just f) -> pure $ Just f
      (Nothing, Nothing) -> pure Nothing

  -- process
  res <- runCabalGildIO mroot opts (cabalGild filepath input)

  case res of
    Right output -> do
      let outputBS = toUTF8BS output
          formatted = outputBS == input

      case optMode opts of
        Mode.Stdout -> BS.putStr outputBS
        Mode.Inplace -> case mfilepath of
          Nothing -> BS.putStr outputBS
          Just _ -> Monad.unless formatted $ BS.writeFile filepath outputBS
        _ -> return ()

      return $ if formatted then Nothing else Just filepath
    Left err -> do
      renderError err
      exitFailure
