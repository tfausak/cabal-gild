-- |
-- License: BSD-3-Clause
-- Copyright: Oleg Grenrus
module CabalGild.Main (main) where

import CabalGild (cabalGild)
import CabalGild.Error (Error (SomeError), renderError)
import CabalGild.Monad (runCabalGildIO)
import CabalGild.Options
import CabalGild.Prelude
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as BS
import Data.Traversable (for)
import qualified Data.Version as Version
import qualified Paths_cabal_gild as This
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import System.Exit (exitFailure)
import qualified System.Exit as Exit
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import qualified Text.Read as Read

main :: IO ()
main = do
  arguments <- Environment.getArgs
  let (flags, filepaths, unknowns, invalids) = GetOpt.getOpt' GetOpt.Permute flagOptions arguments
  mapM_ (Exception.throwM . userError . mappend "unknown option: ") unknowns
  mapM_ (Exception.throwM . userError . mappend "invalid option: ") invalids
  config <- flagsToConfig flags
  let opts = configOptions config

  Monad.when (configHelp config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo name flagOptions
    Exit.exitSuccess

  Monad.when (configVersion config) $ do
    putStrLn $ Version.showVersion This.version
    Exit.exitSuccess

  notFormatted <-
    catMaybes <$> case filepaths of
      [] -> fmap pure $ BS.getContents >>= main' opts Nothing
      (_ : _) -> for filepaths $ \filepath -> do
        contents <- BS.readFile filepath
        main' opts (Just filepath) contents

  when ((optMode opts == ModeCheck) && not (null notFormatted)) $ do
    for_ notFormatted $ \filepath ->
      hPutStrLn stderr $ "error: Input " <> filepath <> " is not formatted."
    exitFailure

main' :: Options -> Maybe FilePath -> BS.ByteString -> IO (Maybe FilePath)
main' opts mfilepath input = do
  -- name of the input
  let filepath = fromMaybe "<stdin>" mfilepath

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
        ModeStdout -> BS.putStr outputBS
        ModeInplace -> case mfilepath of
          Nothing -> BS.putStr outputBS
          Just _ -> unless formatted $ BS.writeFile filepath outputBS
        _ -> return ()

      return $ if formatted then Nothing else Just filepath
    Left err -> do
      renderError err
      exitFailure

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

data Flag
  = FlagCabalFile Bool
  | FlagError Bool
  | FlagHelp
  | FlagIndent String
  | FlagMode Mode
  | FlagStdinInputFile String
  | FlagTabular Bool
  | FlagVersion
  deriving (Eq, Show)

flagOptions :: [GetOpt.OptDescr Flag]
flagOptions =
  [ GetOpt.Option [] ["Werror"] (GetOpt.NoArg $ FlagError True) "Treat warnings as errors",
    GetOpt.Option [] ["Wno-error"] (GetOpt.NoArg $ FlagError False) "",
    GetOpt.Option [] ["indent"] (GetOpt.ReqArg FlagIndent "N") "Indentation",
    GetOpt.Option [] ["tabular"] (GetOpt.NoArg $ FlagTabular True) "Tabular formatting",
    GetOpt.Option [] ["no-tabular"] (GetOpt.NoArg $ FlagTabular False) "",
    GetOpt.Option [] ["cabal-file"] (GetOpt.NoArg $ FlagCabalFile True) "",
    GetOpt.Option ['n'] ["no-cabal-file"] (GetOpt.NoArg $ FlagCabalFile False) "Don't parse as .cabal file",
    GetOpt.Option [] ["stdout"] (GetOpt.NoArg $ FlagMode ModeStdout) "Write output to stdout (default)",
    GetOpt.Option ['i'] ["inplace"] (GetOpt.NoArg $ FlagMode ModeInplace) "Process files in-place",
    GetOpt.Option ['c'] ["check"] (GetOpt.NoArg $ FlagMode ModeCheck) "Fail with non-zero exit code if input is not formatted",
    GetOpt.Option [] ["stdin-input-file"] (GetOpt.ReqArg FlagStdinInputFile "FILE") "When reading from STDIN, use this file path to resolve relative references",
    GetOpt.Option ['h'] ["help"] (GetOpt.NoArg FlagHelp) "Show this help text",
    GetOpt.Option [] ["version"] (GetOpt.NoArg FlagVersion) "Show version"
  ]

data Config = Config
  { configHelp :: Bool,
    configOptions :: Options,
    configVersion :: Bool
  }
  deriving (Show)

initialConfig :: Config
initialConfig =
  Config
    { configHelp = False,
      configOptions = defaultOptions,
      configVersion = False
    }

applyFlag :: (Exception.MonadThrow m) => Config -> Flag -> m Config
applyFlag c f = case f of
  FlagCabalFile b -> pure c {configOptions = (configOptions c) {optCabalFile = b}}
  FlagError b -> pure c {configOptions = (configOptions c) {optError = b}}
  FlagHelp -> pure c {configHelp = True}
  FlagIndent s -> do
    i <- case Read.readMaybe s of
      Nothing -> Exception.throwM . userError $ "invalid indent: " <> show s
      Just i -> pure i
    pure c {configOptions = (configOptions c) {optIndent = i}}
  FlagMode m -> pure c {configOptions = (configOptions c) {optMode = m}}
  FlagStdinInputFile s -> pure c {configOptions = (configOptions c) {optStdinInputFile = Just s}}
  FlagTabular b -> pure c {configOptions = (configOptions c) {optTabular = b}}
  FlagVersion -> pure c {configVersion = True}

flagsToConfig :: (Exception.MonadThrow m) => [Flag] -> m Config
flagsToConfig = Monad.foldM applyFlag initialConfig
