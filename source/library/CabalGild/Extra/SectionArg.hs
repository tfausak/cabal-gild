module CabalGild.Extra.SectionArg where

import qualified Distribution.Fields as Fields

annotation :: Fields.SectionArg a -> a
annotation sa = case sa of
  Fields.SecArgName x _ -> x
  Fields.SecArgStr x _ -> x
  Fields.SecArgOther x _ -> x
