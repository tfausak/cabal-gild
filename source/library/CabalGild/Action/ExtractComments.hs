module CabalGild.Action.ExtractComments where

import qualified CabalGild.Type.Comment as Comment
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.ByteString.Internal as ByteStringInternal
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified Distribution.Parsec.Position as Position

fromByteString :: ByteString.ByteString -> [Comment.Comment Position.Position]
fromByteString =
  Maybe.mapMaybe (uncurry fromLine)
    . zip [1 ..]
    . Latin1.lines

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

breakComment :: ByteString.ByteString -> (ByteString.ByteString, ByteString.ByteString)
breakComment = ByteString.breakSubstring Comment.delimiter

isBlank :: Word.Word8 -> Bool
isBlank = ByteStringInternal.isSpaceWord8
