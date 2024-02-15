module CabalGild.Type.Comment where

import qualified CabalGild.Extra.String as String
import qualified Data.ByteString as ByteString

data Comment a = Comment
  { annotation :: a,
    value :: ByteString.ByteString
  }
  deriving (Eq, Show)

delimiter :: ByteString.ByteString
delimiter = String.toUtf8 "--"
