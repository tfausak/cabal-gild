module CabalGild.Unstable.Extra.SectionArg where

import qualified Distribution.Fields as Fields

-- | Extracts the annotation from the given 'Fields.SectionArg'.
annotation :: Fields.SectionArg a -> a
annotation sa = case sa of
  Fields.SecArgName x _ -> x
  Fields.SecArgStr x _ -> x
  Fields.SecArgOther x _ -> x
