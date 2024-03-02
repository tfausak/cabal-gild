module CabalGild.Unstable.Extra.FieldLine where

import qualified Data.ByteString as ByteString
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec.FieldLineStream as FieldLineStream

-- | Extracts the annotation of a 'Fields.FieldLine'.
annotation :: Fields.FieldLine a -> a
annotation (Fields.FieldLine x _) = x

-- | Extracts the value of a 'Fields.FieldLine'.
value :: Fields.FieldLine a -> ByteString.ByteString
value (Fields.FieldLine _ x) = x

-- | Converts a list of 'Fields.FieldLine's into a
-- 'FieldLineStream.FieldLineStream'.
toFieldLineStream :: [Fields.FieldLine a] -> FieldLineStream.FieldLineStream
toFieldLineStream fls = case fls of
  [] -> FieldLineStream.FLSLast ByteString.empty
  [x] -> FieldLineStream.FLSLast $ value x
  x : xs -> FieldLineStream.FLSCons (value x) $ toFieldLineStream xs
