module CabalGild.Unstable.Action.AttachComments where

import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified Control.Monad.Trans.State as StateT
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec.Position as Position

-- | High level wrapper around 'fields' that makes this action easier to compose
-- with other actions.
run ::
  (Applicative m) =>
  ([Fields.Field Position.Position], [Comment.Comment Position.Position]) ->
  m ([Fields.Field (Position.Position, Comments.Comments Position.Position)], [Comment.Comment Position.Position])
run (fs, cs) = pure $ StateT.runState (fields Nothing fs) cs

-- | Attaches comments to a list of fields, with awareness of sibling positions.
-- The first argument is the boundary position (e.g., from a parent's next sibling).
fields ::
  Maybe Position.Position ->
  [Fields.Field Position.Position] ->
  StateT.State [Comment.Comment Position.Position] [Fields.Field (Position.Position, Comments.Comments Position.Position)]
fields _ [] = pure []
fields boundary (f : rest) = do
  let nextPos = case rest of
        [] -> boundary
        (next : _) -> Just $ fieldPosition next
  f' <- field nextPos f
  rest' <- fields boundary rest
  pure (f' : rest')

-- | Gets the position of a field (from its name).
fieldPosition :: Fields.Field Position.Position -> Position.Position
fieldPosition f = case f of
  Fields.Field n _ -> Name.annotation n
  Fields.Section n _ _ -> Name.annotation n

-- | Attaches comments to a single field. It is assumed that both the fields
-- and comments are already sorted by their position. This precondition is
-- not checked. Note that comments actually end up attached to the field's
-- name. That's because the 'Field.Field' type doesn't have any annotations
-- directly on it.
--
-- Comments that are indented beyond the field's starting column are attached
-- as trailing comments (in 'Comments.after'), but only if they appear before
-- the next sibling's position.
field ::
  Maybe Position.Position ->
  Fields.Field Position.Position ->
  StateT.State [Comment.Comment Position.Position] (Fields.Field (Position.Position, Comments.Comments Position.Position))
field boundary f = case f of
  Fields.Field n fls -> do
    let col = Position.positionCol $ Name.annotation n
    n' <- name n
    fls' <- traverse fieldLine fls
    trailing <- attachTrailing col boundary
    pure $ Fields.Field (addAfterComments n' trailing) fls'
  Fields.Section n sas fs -> do
    let col = Position.positionCol $ Name.annotation n
    n' <- name n
    sas' <- traverse sectionArg sas
    fs' <- fields boundary fs
    trailing <- attachTrailing col boundary
    pure $ Fields.Section (addAfterComments n' trailing) sas' fs'

-- | Attaches comments to a name. Note that this could be a field name or a
-- section name.
name ::
  Fields.Name Position.Position ->
  StateT.State [Comment.Comment Position.Position] (Fields.Name (Position.Position, Comments.Comments Position.Position))
name (Fields.Name p fn) =
  Fields.Name
    <$> toPosition p
    <*> pure fn

-- | Attach comments to a field line.
fieldLine ::
  Fields.FieldLine Position.Position ->
  StateT.State [Comment.Comment Position.Position] (Fields.FieldLine (Position.Position, Comments.Comments Position.Position))
fieldLine (Fields.FieldLine p bs) =
  Fields.FieldLine
    <$> toPosition p
    <*> pure bs

-- | Attaches comments to a section argument. Note that section arguments
-- cannot actually have comments attached. That's because section arguments
-- must be on the same line as the section name, so all comments will end up
-- attached to the name.
sectionArg ::
  Fields.SectionArg Position.Position ->
  StateT.State [Comment.Comment Position.Position] (Fields.SectionArg (Position.Position, Comments.Comments Position.Position))
sectionArg sa = case sa of
  Fields.SecArgName p bs ->
    Fields.SecArgName
      <$> toPosition p
      <*> pure bs
  Fields.SecArgStr p bs ->
    Fields.SecArgStr
      <$> toPosition p
      <*> pure bs
  Fields.SecArgOther p bs ->
    Fields.SecArgOther
      <$> toPosition p
      <*> pure bs

-- | Attaches comments to a position. This is the workhorse of the module.
-- Comments are attached when their position is less than or equal to the given
-- position. The comments are removed from the state as they are attached.
toPosition ::
  Position.Position ->
  StateT.State [Comment.Comment Position.Position] (Position.Position, Comments.Comments Position.Position)
toPosition p = do
  cs <- StateT.get
  let (xs, ys) = span ((<= p) . Comment.annotation) cs
  StateT.put ys
  pure (p, Comments.MkComments {Comments.before = xs, Comments.after = []})

-- | Attaches trailing comments based on column indentation. Comments that are
-- indented beyond the given column are considered trailing, but only up to
-- the boundary position (if specified). Comments are removed from the state.
attachTrailing ::
  Int ->
  Maybe Position.Position ->
  StateT.State [Comment.Comment Position.Position] [Comment.Comment Position.Position]
attachTrailing col boundary = do
  cs <- StateT.get
  let (trailing, remaining) = span isTrailing cs
  StateT.put remaining
  pure trailing
  where
    isTrailing c =
      let pos = Comment.annotation c
       in Position.positionCol pos > col
            && maybe True (pos <) boundary

-- | Adds trailing comments to the 'after' field of a Name's Comments.
addAfterComments ::
  Fields.Name (Position.Position, Comments.Comments Position.Position) ->
  [Comment.Comment Position.Position] ->
  Fields.Name (Position.Position, Comments.Comments Position.Position)
addAfterComments (Fields.Name (p, cs) fn) trailing =
  Fields.Name (p, cs {Comments.after = Comments.after cs <> trailing}) fn
