module CabalGild.Unstable.Extra.SectionArg where

import qualified Data.ByteString as ByteString
import qualified Distribution.Fields as Fields

-- | Extracts the annotation from the given 'Fields.SectionArg'.
annotation :: Fields.SectionArg a -> a
annotation sa = case sa of
  Fields.SecArgName x _ -> x
  Fields.SecArgStr x _ -> x
  Fields.SecArgOther x _ -> x

-- | Extracts the value from the given 'Fields.SectionArg'.
value :: Fields.SectionArg a -> ByteString.ByteString
value sa = case sa of
  Fields.SecArgName _ x -> x
  Fields.SecArgStr _ x -> x
  Fields.SecArgOther _ x -> x
