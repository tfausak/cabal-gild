module CabalGild.Type.Line where

import qualified CabalGild.Type.Chunk as Chunk
import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens

data Line = Line
  { indent :: Int,
    chunk :: Chunk.Chunk
  }
  deriving (Eq, Show)

chunkLens :: Lens.Lens' Line Chunk.Chunk
chunkLens f s = fmap (\a -> s {chunk = a}) . f $ chunk s

empty :: Line
empty =
  Line
    { indent = 0,
      chunk = mempty
    }

isEmpty :: Line -> Bool
isEmpty = Chunk.isEmpty . chunk

toByteString :: Line -> ByteString.ByteString
toByteString l =
  if isEmpty l
    then ByteString.empty
    else
      mappend (ByteString.replicate (2 * indent l) 0x20)
        . Chunk.value
        $ chunk l
