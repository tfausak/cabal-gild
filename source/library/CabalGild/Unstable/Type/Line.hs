module CabalGild.Unstable.Type.Line where

import qualified CabalGild.Unstable.Type.Chunk as Chunk
import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens

-- | A line of text, which is made up of a chunk and an indent level.
data Line = Line
  { -- | Number of times the chunk should be indented.
    indent :: Int,
    chunk :: Chunk.Chunk
  }
  deriving (Eq, Show)

-- | A lens for the 'chunk' field.
chunkLens :: Lens.Lens' Line Chunk.Chunk
chunkLens f s = fmap (\a -> s {chunk = a}) . f $ chunk s

-- | An empty line, which is an empty chunk with no indentation.
empty :: Line
empty =
  Line
    { indent = 0,
      chunk = mempty
    }

-- | Returns 'True' if the line's chunk is empty.
isEmpty :: Line -> Bool
isEmpty = Chunk.isEmpty . chunk

-- | Converts a line to a 'ByteString' by indenting the chunk.
toByteString :: Line -> ByteString.ByteString
toByteString l =
  if isEmpty l
    then ByteString.empty
    else
      mappend (ByteString.replicate (2 * indent l) 0x20)
        . Chunk.value
        $ chunk l
