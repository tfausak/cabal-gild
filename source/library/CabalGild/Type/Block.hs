-- | This module defines the 'Block' data type.
module CabalGild.Type.Block where

import qualified CabalGild.Type.Line as Line
import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens
import Prelude hiding (lines)

-- | A block of text, which is made up of multiple lines and can have blank
-- lines before and/or after it.
data Block = Block
  { -- | Does this block have a blank line before it?
    lineBefore :: Bool,
    lines :: [Line.Line],
    -- | Does this block have a blank line after it?
    lineAfter :: Bool
  }
  deriving (Eq, Show)

-- | Joins two blocks together by adding a blank line between them if
-- necessary. (A blank line is necessary if /either/ block needs a space.) If
-- either block is empty, the other block is returned.
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

-- | The empty block has no lines and also no blank lines before or after.
instance Monoid Block where
  mempty =
    Block
      { lineBefore = False,
        lines = [],
        lineAfter = False
      }

-- | Converts a single line into a block without blank lines before or after.
fromLine :: Line.Line -> Block
fromLine l = mempty {lines = [l]}

-- | Returns 'True' if the block has no lines.
isEmpty :: Block -> Bool
isEmpty = null . lines

-- | A lens for the 'lineAfter' field.
lineAfterLens :: Lens.Lens' Block Bool
lineAfterLens f s = fmap (\a -> s {lineAfter = a}) . f $ lineAfter s

-- | A lens for the 'lineBefore' field.
lineBeforeLens :: Lens.Lens' Block Bool
lineBeforeLens f s = fmap (\a -> s {lineBefore = a}) . f $ lineBefore s

-- | Converts a block into a 'ByteString.ByteString' by joining the lines
-- together with newline characters. Note that unline 'unlines', there will be
-- no trailing newline unless 'lineAfter' is 'True'.
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
