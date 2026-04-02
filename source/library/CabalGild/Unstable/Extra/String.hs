module CabalGild.Unstable.Extra.String where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

toCaseFold :: String -> String
toCaseFold = Text.unpack . Text.toCaseFold . Text.pack

-- | Converts the given 'String' into a UTF-8 encoded 'ByteString.ByteString'.
toUtf8 :: String -> ByteString.ByteString
toUtf8 = Encoding.encodeUtf8 . Text.pack

-- | Converts a UTF-8 encoded 'ByteString.ByteString' into a 'String'.
fromUtf8 :: ByteString.ByteString -> String
fromUtf8 = Text.unpack . Encoding.decodeUtf8Lenient
