module CabalGild.Action.Reindent where

import qualified CabalGild.Extra.Field as Field
import qualified CabalGild.Extra.FieldLine as FieldLine
import qualified CabalGild.Extra.List as List
import qualified CabalGild.Extra.Name as Name
import qualified CabalGild.Extra.String as String
import qualified CabalGild.Type.Comment as Comment
import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec.Position as Position

run ::
  (Applicative m) =>
  CabalSpecVersion.CabalSpecVersion ->
  ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], cs) ->
  m ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], cs)
run csv (fs, cs) = pure (fields csv fs, cs)

fields ::
  CabalSpecVersion.CabalSpecVersion ->
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])]
fields csv fs =
  -- Note that we want to do this after comments have been attached because we
  -- want comments to come as late as possible. In other words, we don't want a
  -- comment attached to a blank line.
  if csv >= CabalSpecVersion.CabalSpecV3_0
    then fmap field fs
    else fs

field ::
  Fields.Field (Position.Position, [Comment.Comment Position.Position]) ->
  Fields.Field (Position.Position, [Comment.Comment Position.Position])
field f = case f of
  Fields.Field n fls ->
    if Set.member (Name.value n) relevantFieldNames && List.compareLength fls 1 == GT
      then Fields.Field n $ fieldLines f fls
      else f
  Fields.Section n sas fs -> Fields.Section n sas $ fmap field fs

relevantFieldNames :: Set.Set Fields.FieldName
relevantFieldNames =
  Set.fromList $
    fmap
      String.toUtf8
      [ "description"
      ]

fieldLines ::
  Fields.Field (Position.Position, [Comment.Comment Position.Position]) ->
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])]
fieldLines f = fixRows . fixCols f

fixRows ::
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])]
fixRows fls = case fls of
  x : y : zs ->
    x
      : fmap rowToFieldLine [fieldLineToLastRow x + 1 .. fieldLineToFirstRow y - 1]
        <> fixRows (y : zs)
  _ -> fls

fixCols ::
  Fields.Field (Position.Position, [Comment.Comment Position.Position]) ->
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])]
fixCols f fls = case fls of
  [] -> fls
  x : xs ->
    let col = foldr (min . fieldLineToCol) (fieldLineToCol x) xs
     in if fieldToRow f == fieldLineToFirstRow x
          then x : fmap (reindent col) xs
          else fmap (reindent col) fls

fieldLineToCol :: Fields.FieldLine (Position.Position, cs) -> Int
fieldLineToCol = Position.positionCol . fst . FieldLine.annotation

fieldLineToFirstRow ::
  Fields.FieldLine (Position.Position, [Comment.Comment Position.Position]) ->
  Int
fieldLineToFirstRow =
  -- A field line's first row might belong to one of its comments.
  Position.positionRow
    . uncurry (foldr (min . Comment.annotation))
    . FieldLine.annotation

fieldLineToLastRow :: Fields.FieldLine (Position.Position, cs) -> Int
fieldLineToLastRow = Position.positionRow . fst . FieldLine.annotation

fieldToRow :: Fields.Field (Position.Position, cs) -> Int
fieldToRow = Position.positionRow . fst . Name.annotation . Field.name

reindent ::
  Int ->
  Fields.FieldLine (Position.Position, cs) ->
  Fields.FieldLine (Position.Position, cs)
reindent col (Fields.FieldLine (p, cs) b) =
  Fields.FieldLine (p, cs) $ ByteString.replicate (Position.positionCol p - col) 0x20 <> b

rowToFieldLine ::
  Int ->
  Fields.FieldLine (Position.Position, [c])
rowToFieldLine r = Fields.FieldLine (Position.Position r 1, []) ByteString.empty
