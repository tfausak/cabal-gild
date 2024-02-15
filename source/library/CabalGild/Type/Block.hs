module CabalGild.Type.Block where

import qualified CabalGild.Type.Line as Line
import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens
import Prelude hiding (lines)

data Block = Block
  { lineBefore :: Bool,
    lines :: [Line.Line],
    lineAfter :: Bool
  }
  deriving (Eq, Show)

instance Semigroup Block where
  x <> y =
    let s = [Line.empty | lineAfter x || lineBefore y]
        z =
          Block
            { lineBefore = lineBefore x,
              lines = lines x <> s <> lines y,
              lineAfter = lineAfter y
            }
     in if isEmpty x then y else if isEmpty y then x else z

instance Monoid Block where
  mempty =
    Block
      { lineBefore = False,
        lines = [],
        lineAfter = False
      }

fromLine :: Line.Line -> Block
fromLine l = mempty {lines = [l]}

isEmpty :: Block -> Bool
isEmpty = null . lines

lineAfterLens :: Lens.Lens' Block Bool
lineAfterLens f s = fmap (\a -> s {lineAfter = a}) . f $ lineAfter s

lineBeforeLens :: Lens.Lens' Block Bool
lineBeforeLens f s = fmap (\a -> s {lineBefore = a}) . f $ lineBefore s

toByteString :: Block -> ByteString.ByteString
toByteString b =
  if isEmpty b
    then ByteString.empty
    else
      ByteString.intercalate (ByteString.singleton 0x0a)
        . fmap Line.toByteString
        $ concat
          [ [Line.empty | lineBefore b],
            lines b,
            [Line.empty | lineAfter b]
          ]
