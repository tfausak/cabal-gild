module CabalGild.Extra.String where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

-- | Converts the given UTF-8 encoded 'ByteString.ByteString' into a 'String'.
-- Any invalid UTF-8 sequences will be replaced with the Unicode replacement
-- character (U+FFFD).
fromUtf8 :: ByteString.ByteString -> String
fromUtf8 = Text.unpack . Encoding.decodeUtf8Lenient

-- | Converts the given 'String' into a UTF-8 encoded 'ByteString.ByteString'.
toUtf8 :: String -> ByteString.ByteString
toUtf8 = Encoding.encodeUtf8 . Text.pack
