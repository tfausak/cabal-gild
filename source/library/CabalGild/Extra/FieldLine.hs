module CabalGild.Extra.FieldLine where

import qualified Data.ByteString as ByteString
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec.FieldLineStream as FieldLineStream

annotation :: Fields.FieldLine a -> a
annotation (Fields.FieldLine x _) = x

value :: Fields.FieldLine a -> ByteString.ByteString
value (Fields.FieldLine _ x) = x

toFieldLineStream :: [Fields.FieldLine a] -> FieldLineStream.FieldLineStream
toFieldLineStream fls = case fls of
  [] -> FieldLineStream.FLSLast ByteString.empty
  [x] -> FieldLineStream.FLSLast $ value x
  x : xs -> FieldLineStream.FLSCons (value x) $ toFieldLineStream xs
