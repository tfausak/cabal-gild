-- | This module defines a type for tracking blank line positions in input.
--
-- Blank lines serve as explicit boundaries between elements in cabal files.
-- A comment separated from an element by a blank line should not be considered
-- a trailing comment of that element.
module CabalGild.Unstable.Type.BlankLines
  ( -- * Types
    BlankLines,

    -- * Construction
    fromByteString,

    -- * Queries
    hasBlankBetween,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import qualified Data.Word as Word

-- | A set of row numbers that contain only whitespace.
--
-- Row numbers are 1-indexed to match 'Distribution.Parsec.Position'.
newtype BlankLines = BlankLines (Set.Set Int)
  deriving (Eq, Show)

-- | Extract blank line positions from the input.
--
-- A line is considered blank if it contains only whitespace characters
-- (spaces and tabs). Empty lines are also blank.
fromByteString :: ByteString.ByteString -> BlankLines
fromByteString input =
  BlankLines
    . Set.fromList
    . fmap fst
    . filter (isBlankLine . snd)
    . zip [1 ..]
    . ByteString.split newline
    $ input
  where
    newline :: Word.Word8
    newline = 0x0a -- '\n'

-- | Check if a line contains only whitespace.
isBlankLine :: ByteString.ByteString -> Bool
isBlankLine = ByteString.all isSpace
  where
    -- Only consider space (0x20) and tab (0x09) as whitespace.
    -- This matches the behavior expected in cabal files.
    isSpace :: Word.Word8 -> Bool
    isSpace w = w == 0x20 || w == 0x09

-- | Check if there is at least one blank line strictly between two rows.
--
-- Returns 'True' if any blank line exists where @startRow < blankRow < endRow@.
-- Returns 'False' if @startRow >= endRow@ or if no blank lines exist between them.
hasBlankBetween ::
  BlankLines ->
  -- | Start row (exclusive)
  Int ->
  -- | End row (exclusive)
  Int ->
  Bool
hasBlankBetween (BlankLines blanks) startRow endRow
  | startRow >= endRow = False
  | otherwise =
      -- Check if any blank line exists in the open interval (startRow, endRow)
      not . Set.null $
        Set.filter (\r -> r > startRow && r < endRow) blanks
