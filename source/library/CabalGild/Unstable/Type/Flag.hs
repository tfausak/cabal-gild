module CabalGild.Unstable.Type.Flag where

import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
import qualified CabalGild.Unstable.Exception.InvalidOption as InvalidOption
import qualified CabalGild.Unstable.Warning.UnexpectedArgument as UnexpectedArgument
import qualified CabalGild.Unstable.Warning.UnknownOption as UnknownOption
import qualified Control.Monad.Catch as Exception
import qualified Data.Foldable as Foldable
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
      ["crlf"]
      (GetOpt.ReqArg CRLF "LENIENCY")
      "Sets the CRLF handling mode. Must be either 'lenient' or 'strict'.\nDefault: 'lenient'",
    GetOpt.Option
      ['i']
      ["input"]
      (GetOpt.ReqArg Input "FILE")
      "Sets the input file. Use '-' for standard input (STDIN).\nDefault: '-'",
    GetOpt.Option
      []
      ["io"]
      (GetOpt.ReqArg IO "FILE")
      "Shortcut for setting both the input and output files.",
    GetOpt.Option
      ['m']
      ["mode"]
      (GetOpt.ReqArg Mode "MODE")
      "Sets the mode. Must be either 'check' or 'format'.\nDefault: 'format'",
    GetOpt.Option
      ['o']
      ["output"]
      (GetOpt.ReqArg Output "FILE")
      "Sets the output file. Use '-' for standard output (STDOUT).\nDefault: '-'",
    GetOpt.Option
      ['s']
      ["stdin"]
      (GetOpt.ReqArg Stdin "FILE")
      "Sets the path to the input file when using STDIN.\nDefault: '.'"
  ]

-- | Converts a list of command line arguments into a list of flags. If there
-- are any invalid options, an exception will be thrown.
fromArguments :: (Exception.MonadThrow m, MonadWarn.MonadWarn m) => [String] -> m [Flag]
fromArguments arguments = do
  let (flgs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute options arguments
  Foldable.traverse_ (MonadWarn.warn . UnexpectedArgument.fromString) args
  Foldable.traverse_ (Exception.throwM . InvalidOption.fromString) errs
  Foldable.traverse_ (MonadWarn.warn . UnknownOption.fromString) opts
  pure flgs
