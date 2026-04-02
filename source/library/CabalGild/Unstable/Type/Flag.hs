{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Type.Flag where

import Bluefin.Eff (Eff, (:>))
import qualified Bluefin.Exception as Exception
import qualified CabalGild.Unstable.Exception.DuplicateOption as DuplicateOption
import qualified CabalGild.Unstable.Exception.InvalidOption as InvalidOption
import qualified CabalGild.Unstable.Exception.UnknownOption as UnknownOption
import qualified Control.Exception as E
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
      "Deprecated. Sets the input file. Use a positional argument instead.",
    GetOpt.Option
      []
      [ioOption]
      (GetOpt.ReqArg IO "FILE")
      "Deprecated. Shortcut for setting both the input and output files.\nUse a positional argument instead.",
    GetOpt.Option
      ['m']
      [modeOption]
      (GetOpt.ReqArg Mode "MODE")
      "Sets the mode. Must be either 'check' or 'format'.\nDefault: 'format'",
    GetOpt.Option
      ['o']
      [outputOption]
      (GetOpt.ReqArg Output "FILE")
      "Deprecated. Sets the output file. Use piping instead.",
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

-- | Converts a list of command line arguments into a list of flags and
-- positional arguments. If there are any invalid options or unknown options, an
-- exception will be thrown.
fromArguments :: (eX :> es) => Exception.Exception E.SomeException eX -> [String] -> Eff es ([Flag], [String])
fromArguments ex arguments = do
  let (flgs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute options arguments
  Foldable.traverse_ (Exception.throw ex . E.toException . InvalidOption.fromString) errs
  Foldable.traverse_ (Exception.throw ex . E.toException . UnknownOption.fromString) opts
  detectDuplicateOptions ex flgs
  pure (flgs, args)

detectDuplicateOptions :: (eX :> es) => Exception.Exception E.SomeException eX -> [Flag] -> Eff es ()
detectDuplicateOptions ex =
  let toWarnings o l =
        fmap (uncurry . flip $ DuplicateOption.DuplicateOption o)
          . reverse
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
   in Foldable.traverse_ (Exception.throw ex . E.toException)
        . concatMap (uncurry toWarnings)
        . Map.toAscList
        . Map.fromListWith (<>)
        . Maybe.mapMaybe fromFlag
