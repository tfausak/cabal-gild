module CabalGild.Type.Chunk where

import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens

data Chunk = Chunk
  { spaceBefore :: Bool,
    value :: ByteString.ByteString,
    spaceAfter :: Bool
  }
  deriving (Eq, Show)

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

instance Monoid Chunk where
  mempty =
    Chunk
      { spaceBefore = False,
        value = ByteString.empty,
        spaceAfter = False
      }

colon :: Chunk
colon = Lens.set spaceAfterLens True . fromByteString $ ByteString.singleton 0x3a

fromByteString :: ByteString.ByteString -> Chunk
fromByteString bs = mempty {value = bs}

isEmpty :: Chunk -> Bool
isEmpty = ByteString.null . value

spaceAfterLens :: Lens.Lens' Chunk Bool
spaceAfterLens f s = fmap (\a -> s {spaceAfter = a}) . f $ spaceAfter s

spaceBeforeLens :: Lens.Lens' Chunk Bool
spaceBeforeLens f s = fmap (\a -> s {spaceBefore = a}) . f $ spaceBefore s
