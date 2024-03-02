-- | This module defines the 'Comment' data type.
module CabalGild.Unstable.Type.Comment where

import qualified CabalGild.Unstable.Extra.String as String
import qualified Data.ByteString as ByteString

-- | A comment with corresponding annotation. Leading blank space is preserved,
-- but trailing blank space is not.
data Comment a = Comment
  { annotation :: a,
    -- | Does /not/ include the 'delimiter'.
    value :: ByteString.ByteString
  }
  deriving (Eq, Show)

-- | The start of a comment, which is @--@.
delimiter :: ByteString.ByteString
delimiter = String.toUtf8 "--"
