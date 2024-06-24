module CabalGild.Unstable.Type.Flag where

import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
import qualified CabalGild.Unstable.Exception.InvalidOption as InvalidOption
import qualified CabalGild.Unstable.Warning.DuplicateOption as DuplicateOption
import qualified CabalGild.Unstable.Warning.UnexpectedArgument as UnexpectedArgument
import qualified CabalGild.Unstable.Warning.UnknownOption as UnknownOption
import qualified Control.Monad.Catch as Exception
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.Console.GetOpt as GetOpt

-- | A flag, which represents a command line option. The values associated with
-- the flags are typically not parsed. These just handle the structure of
-- command line options.
data Flag
  = CRLF String
  | Help Bool
  | Input String
  | IO String
  | Mode String
  | Output String
  | Stdin String
  | Version Bool
  deriving (Eq, Show)

-- | The command line options the correspond to the flags.
options :: [GetOpt.OptDescr Flag]
options =
  [ GetOpt.Option
      ['h', '?']
      ["help"]
      (GetOpt.NoArg $ Help True)
      "Shows this help message.",
    GetOpt.Option
      []
      ["no-help"]
      (GetOpt.NoArg $ Help False)
      "",
    GetOpt.Option
      ['v']
      ["version"]
      (GetOpt.NoArg $ Version True)
      "Shows the version number.",
    GetOpt.Option
      []
      ["no-version"]
      (GetOpt.NoArg $ Version False)
      "",
    GetOpt.Option
      []
      [crlfOption]
      (GetOpt.ReqArg CRLF "LENIENCY")
      "Sets the CRLF handling mode. Must be either 'lenient' or 'strict'.\nDefault: 'lenient'",
    GetOpt.Option
      ['i']
      [inputOption]
      (GetOpt.ReqArg Input "FILE")
      "Sets the input file. Use '-' for standard input (STDIN).\nDefault: '-'",
    GetOpt.Option
      []
      [ioOption]
      (GetOpt.ReqArg IO "FILE")
      "Shortcut for setting both the input and output files.",
    GetOpt.Option
      ['m']
      [modeOption]
      (GetOpt.ReqArg Mode "MODE")
      "Sets the mode. Must be either 'check' or 'format'.\nDefault: 'format'",
    GetOpt.Option
      ['o']
      [outputOption]
      (GetOpt.ReqArg Output "FILE")
      "Sets the output file. Use '-' for standard output (STDOUT).\nDefault: '-'",
    GetOpt.Option
      ['s']
      [stdinOption]
      (GetOpt.ReqArg Stdin "FILE")
      "Sets the path to the input file when using STDIN.\nDefault: '.'"
  ]

crlfOption :: String
crlfOption = "crlf"

inputOption :: String
inputOption = "input"

ioOption :: String
ioOption = "io"

modeOption :: String
modeOption = "mode"

outputOption :: String
outputOption = "output"

stdinOption :: String
stdinOption = "stdin"

-- | Converts a list of command line arguments into a list of flags. If there
-- are any invalid options, an exception will be thrown.
fromArguments :: (Exception.MonadThrow m, MonadWarn.MonadWarn m) => [String] -> m [Flag]
fromArguments arguments = do
  let (flgs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute options arguments
  Foldable.traverse_ (MonadWarn.warn . UnexpectedArgument.fromString) args
  Foldable.traverse_ (MonadWarn.warn . UnknownOption.fromString) opts
  emitWarnings flgs
  Foldable.traverse_ (Exception.throwM . InvalidOption.fromString) errs
  pure flgs

emitWarnings :: (MonadWarn.MonadWarn m) => [Flag] -> m ()
emitWarnings =
  let toWarnings o l =
        fmap (uncurry . flip $ DuplicateOption.DuplicateOption o)
          . filter (uncurry (/=))
          $ zip l (drop 1 l)
      fromFlag f = case f of
        CRLF s -> Just (crlfOption, [s])
        Input s -> Just (inputOption, [s])
        IO s -> Just (ioOption, [s])
        Mode s -> Just (modeOption, [s])
        Output s -> Just (outputOption, [s])
        Stdin s -> Just (stdinOption, [s])
        _ -> Nothing
   in Foldable.traverse_ MonadWarn.warn
        . concatMap (uncurry toWarnings)
        . Map.toAscList
        . Map.fromListWith (<>)
        . Maybe.mapMaybe fromFlag
