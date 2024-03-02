module CabalGild.Unstable.Extra.String where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

-- | Converts the given 'String' into a UTF-8 encoded 'ByteString.ByteString'.
toUtf8 :: String -> ByteString.ByteString
toUtf8 = Encoding.encodeUtf8 . Text.pack
