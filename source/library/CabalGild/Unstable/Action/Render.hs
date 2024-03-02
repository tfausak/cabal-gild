module CabalGild.Unstable.Action.Render where

import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.SectionArg as SectionArg
import qualified CabalGild.Unstable.Type.Block as Block
import qualified CabalGild.Unstable.Type.Chunk as Chunk
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Line as Line
import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Fields as Fields

-- | A wrapper around 'toByteString' to allow this to be composed with other
-- actions.
run ::
  (Applicative m) =>
  ([Fields.Field [Comment.Comment a]], [Comment.Comment a]) ->
  m ByteString.ByteString
run = pure . uncurry toByteString

-- | Renders the given fields and comments to a byte string.
toByteString ::
  [Fields.Field [Comment.Comment a]] ->
  [Comment.Comment a] ->
  ByteString.ByteString
toByteString fs cs =
  let i = 0 :: Int
   in Block.toByteString
        . Lens.set Block.lineBeforeLens False
        . Lens.set Block.lineAfterLens True
        $ fields i fs <> comments i cs

-- | Renders the given fields to a block at the given indentation level.
fields :: Int -> [Fields.Field [Comment.Comment a]] -> Block.Block
fields = foldMap . field

-- | Renders the given field to a block at the given indentation level.
--
-- If a field only has one line and no comments, then it can be rendered all on
-- one line.
field :: Int -> Fields.Field [Comment.Comment a] -> Block.Block
field i f = case f of
  Fields.Field n fls -> case fls of
    [fl]
      | null $ FieldLine.annotation fl ->
          comments i (Name.annotation n)
            <> ( Block.fromLine
                   . Lens.over Line.chunkLens (mappend $ name n <> Chunk.colon)
                   $ fieldLine i fl
               )
    _ ->
      Lens.set Block.lineAfterLens True $
        comments i (Name.annotation n)
          <> Block.fromLine
            Line.Line
              { Line.indent = i,
                Line.chunk = name n <> Chunk.colon
              }
          <> fieldLines (i + 1) fls
  Fields.Section n sas fs ->
    Lens.set Block.lineBeforeLens (not $ Name.isElif n || Name.isElse n)
      . Lens.set Block.lineAfterLens (not $ Name.isIf n || Name.isElif n)
      $ comments i (Name.annotation n)
        <> comments i (concatMap SectionArg.annotation sas)
        <> Block.fromLine
          Line.Line
            { Line.indent = i,
              Line.chunk = Lens.set Chunk.spaceAfterLens True (name n) <> sectionArgs sas
            }
        <> Lens.set Block.lineBeforeLens False (fields (i + 1) fs)

-- | Renders the given name to a chunk.
name :: Fields.Name a -> Chunk.Chunk
name = Chunk.fromByteString . Name.value

-- | Renders the given field lines to a block at the given indentation level.
fieldLines :: Int -> [Fields.FieldLine [Comment.Comment a]] -> Block.Block
fieldLines = foldMap . fieldLineC

-- | Renders the given field line and its comments to a block at the given
-- indentation level.
fieldLineC :: Int -> Fields.FieldLine [Comment.Comment a] -> Block.Block
fieldLineC i fl =
  comments i (FieldLine.annotation fl)
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
sectionArg sa = case sa of
  Fields.SecArgName _ bs ->
    Lens.set Chunk.spaceBeforeLens True
      . Lens.set Chunk.spaceAfterLens True
      $ Chunk.fromByteString bs
  Fields.SecArgStr _ bs ->
    Lens.set Chunk.spaceBeforeLens True
      . Lens.set Chunk.spaceAfterLens True
      . Chunk.fromByteString
      . flip ByteString.snoc 0x22
      $ ByteString.cons 0x22 bs
  Fields.SecArgOther _ bs ->
    let b =
          bs /= ByteString.singleton 0x21 -- !
            && bs /= ByteString.singleton 0x28 -- (
            && bs /= ByteString.singleton 0x29 -- )
     in Lens.set Chunk.spaceBeforeLens b
          . Lens.set Chunk.spaceAfterLens b
          $ Chunk.fromByteString bs

-- | Renders the given comments to a block at the given indentation level.
comments :: Int -> [Comment.Comment a] -> Block.Block
comments i cs = mempty {Block.lines = fmap (comment i) cs}

-- | Renders the given comment to a line at the given indentation level.
comment :: Int -> Comment.Comment a -> Line.Line
comment i =
  Line.Line i
    . Chunk.fromByteString
    . mappend Comment.delimiter
    . Comment.value
