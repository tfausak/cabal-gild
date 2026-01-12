module CabalGild.Unstable.Action.ExtractComments where

import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.ByteString.Internal as ByteStringInternal
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified Distribution.Parsec.Position as Position

-- | Extracts comments from the given byte string. This is a wrapper around
-- 'fromLine', where lines are split using 'Latin1.lines'.
fromByteString :: ByteString.ByteString -> [Comment.Comment Position.Position]
fromByteString =
  Maybe.mapMaybe (uncurry fromLine)
    . zip [1 ..]
    . Latin1.lines

-- | Extracts a comment from the given line. If the line does not contain a
-- comment, the result will be 'Alternative.empty'.
fromLine ::
  (Applicative.Alternative m, Monad m) =>
  Int ->
  ByteString.ByteString ->
  m (Comment.Comment Position.Position)
fromLine row line = do
  let (before, after) = breakComment line
  rest <-
    maybe Applicative.empty pure $
      ByteString.stripPrefix Comment.delimiter after
  Monad.guard $ ByteString.all isBlank before
  pure
    Comment.Comment
      { Comment.annotation = Position.Position row $ 1 + ByteString.length before,
        Comment.value = ByteString.dropWhileEnd isBlank rest
      }

-- | Breaks a byte string into two parts: the part before the comment delimiter
-- and the part after. If there is no comment, the part after will be empty.
breakComment :: ByteString.ByteString -> (ByteString.ByteString, ByteString.ByteString)
breakComment = ByteString.breakSubstring Comment.delimiter

-- | Returns true if the given byte is a blank character. Currently this is a
-- wrapper around 'ByteStringInternal.isSpaceWord8'. Perhaps it should only
-- check for spaces and tabs though.
isBlank :: Word.Word8 -> Bool
isBlank = ByteStringInternal.isSpaceWord8
