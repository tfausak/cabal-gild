-- | This module defines the 'Chunk' data type.
module CabalGild.Unstable.Type.Chunk where

import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens

-- | A chunk of text, which is made up of a byte string and can have blank
-- spaces before and/or after it.
data Chunk = Chunk
  { -- | Does this chunk have a blank space before it?
    spaceBefore :: Bool,
    value :: ByteString.ByteString,
    -- | Does this chunk have a blank space after it?
    spaceAfter :: Bool
  }
  deriving (Eq, Show)

-- | Joins two chunks together by adding a blank space between them if
-- necessary. (A blank space is necessary if /both/ chunks need a space.) If
-- either chunk is empty, the other chunk is returned.
instance Semigroup Chunk where
  x <> y =
    let s =
          if spaceAfter x && spaceBefore y
            then ByteString.singleton 0x20
            else ByteString.empty
        z =
          Chunk
            { spaceBefore = spaceBefore x,
              value = value x <> s <> value y,
              spaceAfter = spaceAfter y
            }
     in if isEmpty x then y else if isEmpty y then x else z

-- | The empty chunk has no value and also no blank spaces before or after.
instance Monoid Chunk where
  mempty =
    Chunk
      { spaceBefore = False,
        value = ByteString.empty,
        spaceAfter = False
      }

-- | A colon with a space after.
colon :: Chunk
colon = Lens.set spaceAfterLens True . fromByteString $ ByteString.singleton 0x3a

-- | Converts a byte string into a chunk without blank spaces before or after.
fromByteString :: ByteString.ByteString -> Chunk
fromByteString bs = mempty {value = bs}

-- | Returns 'True' if the chunk's byte string is empty.
isEmpty :: Chunk -> Bool
isEmpty = ByteString.null . value

-- | A lens for the 'spaceAfter' field.
spaceAfterLens :: Lens.Lens' Chunk Bool
spaceAfterLens f s = fmap (\a -> s {spaceAfter = a}) . f $ spaceAfter s

-- | A lens for the 'spaceBefore' field.
spaceBeforeLens :: Lens.Lens' Chunk Bool
spaceBeforeLens f s = fmap (\a -> s {spaceBefore = a}) . f $ spaceBefore s
