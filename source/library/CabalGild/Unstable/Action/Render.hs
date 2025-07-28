module CabalGild.Unstable.Action.Render where

import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.SectionArg as SectionArg
import qualified CabalGild.Unstable.Type.Block as Block
import qualified CabalGild.Unstable.Type.Chunk as Chunk
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Line as Line
import qualified Data.ByteString as ByteString
import qualified Data.Function as Function
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec.Position as Position

-- | A wrapper around 'toByteString' to allow this to be composed with other
-- actions.
run ::
  (Applicative m) =>
  CabalSpecVersion.CabalSpecVersion ->
  ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], [Comment.Comment Position.Position]) ->
  m ByteString.ByteString
run csv = pure . uncurry (toByteString csv)

-- | Renders the given fields and comments to a byte string.
toByteString ::
  CabalSpecVersion.CabalSpecVersion ->
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])] ->
  [Comment.Comment Position.Position] ->
  ByteString.ByteString
toByteString csv fs cs =
  let i = 0 :: Int
   in Block.toByteString
        . Lens.set Block.lineBeforeLens False
        . Lens.set Block.lineAfterLens True
        $ fields csv i fs <> comments i cs

-- | Renders the given fields to a block at the given indentation level.
fields :: CabalSpecVersion.CabalSpecVersion -> Int -> [Fields.Field (Position.Position, [Comment.Comment Position.Position])] -> Block.Block
fields csv = foldMap . field csv

-- | Renders the given field to a block at the given indentation level.
--
-- If a field only has one line and no comments, then it can be rendered all on
-- one line.
field :: CabalSpecVersion.CabalSpecVersion -> Int -> Fields.Field (Position.Position, [Comment.Comment Position.Position]) -> Block.Block
field csv i f = case f of
  Fields.Field n fls -> case fls of
    [fl]
      | null . snd $ FieldLine.annotation fl,
        sameRow (Name.annotation n) (FieldLine.annotation fl) ->
          comments i (snd $ Name.annotation n)
            <> ( Block.fromLine
                   . Lens.over Line.chunkLens (mappend $ name n <> Chunk.colon)
                   $ fieldLine i fl
               )
    _ ->
      Lens.set Block.lineAfterLens (not $ null fls) $
        comments i (snd $ Name.annotation n)
          <> Block.fromLine
            Line.Line
              { Line.indent = i,
                Line.chunk = name n <> Chunk.colon
              }
          <> fieldLines (i + 1) fls
  Fields.Section n sas fs ->
    Lens.set Block.lineBeforeLens (not $ Name.isElif csv n || Name.isElse n)
      . Lens.set Block.lineAfterLens (not $ Name.isIf n || Name.isElif csv n)
      $ comments i (snd $ Name.annotation n)
        <> comments i (concatMap (snd . SectionArg.annotation) sas)
        <> Block.fromLine
          Line.Line
            { Line.indent = i,
              Line.chunk = Lens.set Chunk.spaceAfterLens True (name n) <> sectionArgs sas
            }
        <> Lens.set Block.lineBeforeLens False (fields csv (i + 1) fs)

-- | Returns true if the two positions are on the same row.
sameRow :: (Position.Position, cs) -> (Position.Position, cs) -> Bool
sameRow = Function.on (==) $ Position.positionRow . fst

-- | Renders the given name to a chunk.
name :: Fields.Name a -> Chunk.Chunk
name = Chunk.fromByteString . Name.value

-- | Renders the given field lines to a block at the given indentation level.
fieldLines :: Int -> [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])] -> Block.Block
fieldLines = foldMap . fieldLineC

-- | Renders the given field line and its comments to a block at the given
-- indentation level.
fieldLineC :: Int -> Fields.FieldLine (Position.Position, [Comment.Comment Position.Position]) -> Block.Block
fieldLineC i fl =
  comments i (snd $ FieldLine.annotation fl)
    <> Block.fromLine (fieldLine i fl)

-- | Renders the given field line to a line at the given indentation level.
fieldLine :: Int -> Fields.FieldLine a -> Line.Line
fieldLine i =
  Line.Line i
    . Lens.set Chunk.spaceBeforeLens True
    . Chunk.fromByteString
    . FieldLine.value

-- | Renders the given section arguments to a chunk. Note that comments are
-- ignored. In practice this isn't a problem because section arguments can't
-- have comments attached anyway.
sectionArgs :: [Fields.SectionArg a] -> Chunk.Chunk
sectionArgs = Lens.set Chunk.spaceBeforeLens True . foldMap sectionArg

-- | Renders the given section argument to a chunk.
sectionArg :: Fields.SectionArg a -> Chunk.Chunk
sectionArg sa = Lens.set Chunk.spaceBeforeLens True
  . Lens.set Chunk.spaceAfterLens True
  . Chunk.fromByteString
  $ case sa of
    Fields.SecArgName _ bs -> bs
    Fields.SecArgStr _ bs -> flip ByteString.snoc 0x22 $ ByteString.cons 0x22 bs
    Fields.SecArgOther _ bs -> bs

-- | Renders the given comments to a block at the given indentation level.
comments :: Int -> [Comment.Comment Position.Position] -> Block.Block
comments i cs = mempty {Block.lines = fmap (comment i) cs}

-- | Renders the given comment to a line at the given indentation level.
-- If the comment was originally indented significantly beyond the section level, preserve that indentation.
comment :: Int -> Comment.Comment Position.Position -> Line.Line
comment sectionIndent c =
  let originalCol = Position.positionCol (Comment.annotation c)
      originalIndent = originalCol - 1  -- Convert to 0-indexed
      fieldIndent = sectionIndent + 2
      -- Only preserve original indentation if it's significantly more indented than field level
      -- This is for cases like nix sha256 comments that need to stay within the section
      finalIndent = if originalIndent > fieldIndent && originalIndent >= fieldIndent + 1
                   then originalIndent
                   else sectionIndent
  in Line.Line finalIndent
       . Chunk.fromByteString
       . mappend Comment.delimiter
       . Comment.value
       $ c
