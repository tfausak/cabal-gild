module CabalGild.Type.Flag where

import qualified CabalGild.Type.Mode as Mode
import qualified System.Console.GetOpt as GetOpt

data Flag
  = CabalFile Bool
  | Error Bool
  | Help
  | Indent String
  | Mode Mode.Mode
  | StdinInputFile String
  | Tabular Bool
  | Version
  deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
  [ GetOpt.Option [] ["Werror"] (GetOpt.NoArg $ Error True) "Treat warnings as errors",
    GetOpt.Option [] ["Wno-error"] (GetOpt.NoArg $ Error False) "",
    GetOpt.Option [] ["indent"] (GetOpt.ReqArg Indent "N") "Indentation",
    GetOpt.Option [] ["tabular"] (GetOpt.NoArg $ Tabular True) "Tabular formatting",
    GetOpt.Option [] ["no-tabular"] (GetOpt.NoArg $ Tabular False) "",
    GetOpt.Option [] ["cabal-file"] (GetOpt.NoArg $ CabalFile True) "",
    GetOpt.Option ['n'] ["no-cabal-file"] (GetOpt.NoArg $ CabalFile False) "Don't parse as .cabal file",
    GetOpt.Option [] ["stdout"] (GetOpt.NoArg $ Mode Mode.Stdout) "Write output to stdout (default)",
    GetOpt.Option ['i'] ["inplace"] (GetOpt.NoArg $ Mode Mode.Inplace) "Process files in-place",
    GetOpt.Option ['c'] ["check"] (GetOpt.NoArg $ Mode Mode.Check) "Fail with non-zero exit code if input is not formatted",
    GetOpt.Option [] ["stdin-input-file"] (GetOpt.ReqArg StdinInputFile "FILE") "When reading from STDIN, use this file path to resolve relative references",
    GetOpt.Option ['h'] ["help"] (GetOpt.NoArg Help) "Show this help text",
    GetOpt.Option [] ["version"] (GetOpt.NoArg Version) "Show version"
  ]
