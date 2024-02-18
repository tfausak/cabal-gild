module CabalGild.Action.ReflowText where

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

-- | A wrapper around 'fields' to allow this to be composed with other actions.
run ::
  (Applicative m) =>
  CabalSpecVersion.CabalSpecVersion ->
  ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], cs) ->
  m ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], cs)
run csv (fs, cs) = pure (fields csv fs, cs)

-- | Reflows the free text field values if the Cabal spec version is recent
-- enough (at least @3.0@).
--
-- Note that this requires comments to be already attached. That's because
-- comments should not be attached to blank lines, which this function will
-- insert.
fields ::
  CabalSpecVersion.CabalSpecVersion ->
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])]
fields csv fs =
  if csv >= CabalSpecVersion.CabalSpecV3_0
    then fmap field fs
    else fs

-- | Reflows the free text field value if applicable. Otherwise returns the
-- field as is. If the field is a section, the fields within the section will
-- be recursively reflowed.
field ::
  Fields.Field (Position.Position, [Comment.Comment Position.Position]) ->
  Fields.Field (Position.Position, [Comment.Comment Position.Position])
field f = case f of
  Fields.Field n fls ->
    if Set.member (Name.value n) relevantFieldNames && List.compareLength fls 1 == GT
      then Fields.Field n $ fieldLines f fls
      else f
  Fields.Section n sas fs -> Fields.Section n sas $ fmap field fs

-- | The names of the fields that should be reflowed.
relevantFieldNames :: Set.Set Fields.FieldName
relevantFieldNames =
  Set.fromList $
    fmap
      String.toUtf8
      [ "description"
      ]

-- | Reflows the field lines for the given field. This is just a wrapper around
-- 'fixRows' and 'fixCols'.
fieldLines ::
  Fields.Field (Position.Position, [Comment.Comment Position.Position]) ->
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])]
fieldLines f = fixRows . fixCols f

-- | Inserts blank lines between field lines if necessary.
fixRows ::
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.FieldLine (Position.Position, [Comment.Comment Position.Position])]
fixRows fls = case fls of
  x : y : zs ->
    x
      : fmap rowToFieldLine [fieldLineToLastRow x + 1 .. fieldLineToFirstRow y - 1]
        <> fixRows (y : zs)
  _ -> fls

-- | Reindents field lines by finding the least indented line and adjusting the
-- other lines relative to that one. Note that if the first field line is on
-- the same line as the field itself, it will never be reindented.
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

-- | Extracts the column number from a field line.
fieldLineToCol :: Fields.FieldLine (Position.Position, cs) -> Int
fieldLineToCol = Position.positionCol . fst . FieldLine.annotation

-- | Extracts the /first/ row number from a field line, which might belong to
-- one of its comments.
fieldLineToFirstRow ::
  Fields.FieldLine (Position.Position, [Comment.Comment Position.Position]) ->
  Int
fieldLineToFirstRow =
  Position.positionRow
    . uncurry (foldr (min . Comment.annotation))
    . FieldLine.annotation

-- | Extracts the /last/ row number from a field line, which will not belong to
-- any of its comments.
fieldLineToLastRow :: Fields.FieldLine (Position.Position, cs) -> Int
fieldLineToLastRow = Position.positionRow . fst . FieldLine.annotation

-- | Extracts the row number from a field.
fieldToRow :: Fields.Field (Position.Position, cs) -> Int
fieldToRow = Position.positionRow . fst . Name.annotation . Field.name

-- | Reindents the field line using the given column number.
reindent ::
  Int ->
  Fields.FieldLine (Position.Position, cs) ->
  Fields.FieldLine (Position.Position, cs)
reindent col (Fields.FieldLine (p, cs) b) =
  Fields.FieldLine (p, cs) $ ByteString.replicate (Position.positionCol p - col) 0x20 <> b

-- | Creates a blank field line at the given row number.
rowToFieldLine ::
  Int ->
  Fields.FieldLine (Position.Position, [c])
rowToFieldLine r = Fields.FieldLine (Position.Position r 1, []) ByteString.empty
