module CabalGild.Type.Flag where

import qualified CabalGild.Exception.InvalidOption as InvalidOption
import qualified CabalGild.Exception.UnexpectedArgument as UnexpectedArgument
import qualified CabalGild.Exception.UnknownOption as UnknownOption
import qualified Control.Monad.Catch as Exception
import qualified System.Console.GetOpt as GetOpt

data Flag
  = Help Bool
  | Input String
  | Mode String
  | Output String
  | Stdin String
  | Version Bool
  deriving (Eq, Show)

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
      ['i']
      ["input"]
      (GetOpt.ReqArg Input "FILE")
      "Sets the input file. Use '-' for standard input (STDIN).\nDefault: '-'",
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

fromArguments :: (Exception.MonadThrow m) => [String] -> m [Flag]
fromArguments arguments = do
  let (flgs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute options arguments
  mapM_ (Exception.throwM . UnexpectedArgument.fromString) args
  mapM_ (Exception.throwM . InvalidOption.fromString) errs
  mapM_ (Exception.throwM . UnknownOption.fromString) opts
  pure flgs
