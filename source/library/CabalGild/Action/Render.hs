module CabalGild.Action.Render where

import qualified CabalGild.Extra.FieldLine as FieldLine
import qualified CabalGild.Extra.Name as Name
import qualified CabalGild.Extra.SectionArg as SectionArg
import qualified CabalGild.Type.Block as Block
import qualified CabalGild.Type.Chunk as Chunk
import qualified CabalGild.Type.Comment as Comment
import qualified CabalGild.Type.Line as Line
import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Fields as Fields

run ::
  (Applicative m) =>
  ([Fields.Field [Comment.Comment a]], [Comment.Comment a]) ->
  m ByteString.ByteString
run = pure . uncurry toByteString

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

fields :: Int -> [Fields.Field [Comment.Comment a]] -> Block.Block
fields = foldMap . field

field :: Int -> Fields.Field [Comment.Comment a] -> Block.Block
field i f = case f of
  Fields.Field n fls -> case fls of
    [fl]
      | null $ FieldLine.annotation fl ->
          -- If the field only has one line and no comments, then it can be
          -- rendered all on one line.
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
    Lens.set Block.lineBeforeLens True
      . Lens.set Block.lineAfterLens True
      $ comments i (Name.annotation n)
        -- Section arguments should never have comments in practice. This is
        -- here simply to ensure that they aren't lost.
        <> comments i (concatMap SectionArg.annotation sas)
        <> Block.fromLine
          Line.Line
            { Line.indent = i,
              Line.chunk = Lens.set Chunk.spaceAfterLens True (name n) <> sectionArgs sas
            }
        <> Lens.set Block.lineBeforeLens False (fields (i + 1) fs)

name :: Fields.Name a -> Chunk.Chunk
name = Chunk.fromByteString . Name.value

fieldLines :: Int -> [Fields.FieldLine [Comment.Comment a]] -> Block.Block
fieldLines = foldMap . fieldLineC

fieldLineC :: Int -> Fields.FieldLine [Comment.Comment a] -> Block.Block
fieldLineC i fl =
  comments i (FieldLine.annotation fl)
    <> Block.fromLine (fieldLine i fl)

fieldLine :: Int -> Fields.FieldLine a -> Line.Line
fieldLine i =
  Line.Line i
    . Lens.set Chunk.spaceBeforeLens True
    . Chunk.fromByteString
    . FieldLine.value

sectionArgs :: [Fields.SectionArg a] -> Chunk.Chunk
sectionArgs = Lens.set Chunk.spaceBeforeLens True . foldMap sectionArg

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

comments :: Int -> [Comment.Comment a] -> Block.Block
comments i cs = mempty {Block.lines = fmap (comment i) cs}

comment :: Int -> Comment.Comment a -> Line.Line
comment i =
  Line.Line i
    . Chunk.fromByteString
    . mappend Comment.delimiter
    . Comment.value
