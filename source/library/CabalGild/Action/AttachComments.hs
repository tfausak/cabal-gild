module CabalGild.Action.AttachComments where

import qualified CabalGild.Type.Comment as Comment
import qualified Control.Monad.Trans.State as StateT
import qualified Distribution.Fields as Fields

-- | High level wrapper around 'fields' that makes this action easier to
-- compose with other actions.
run ::
  (Applicative m, Ord p) =>
  ([Fields.Field p], [Comment.Comment p]) ->
  m ([Fields.Field (p, [Comment.Comment p])], [Comment.Comment p])
run (fs, cs) = pure $ StateT.runState (fields fs) cs

-- | Given a bunch of fields and comments, attaches each comment to the field
-- that it belongs to. It is assumed that both the fields and comments are
-- already sorted by their position @p@. This precondition is not checked.
fields ::
  (Ord p) =>
  [Fields.Field p] ->
  StateT.State [Comment.Comment p] [Fields.Field (p, [Comment.Comment p])]
fields = traverse field

-- | Attaches comments to a single field. Note that comments actually end up
-- attached to the field's name. That's because the 'Field.Field' type doesn't
-- have any annotations directly on it.
field ::
  (Ord p) =>
  Fields.Field p ->
  StateT.State [Comment.Comment p] (Fields.Field (p, [Comment.Comment p]))
field f = case f of
  Fields.Field n fls ->
    Fields.Field
      <$> name n
      <*> traverse fieldLine fls
  Fields.Section n sas fs ->
    Fields.Section
      <$> name n
      <*> traverse sectionArg sas
      <*> traverse field fs

-- | Attaches comments to a name. Note that this could be a field name or a
-- section name.
name ::
  (Ord p) =>
  Fields.Name p ->
  StateT.State [Comment.Comment p] (Fields.Name (p, [Comment.Comment p]))
name (Fields.Name p fn) =
  Fields.Name
    <$> toPosition p
    <*> pure fn

-- | Attach comments to a field line.
fieldLine ::
  (Ord p) =>
  Fields.FieldLine p ->
  StateT.State [Comment.Comment p] (Fields.FieldLine (p, [Comment.Comment p]))
fieldLine (Fields.FieldLine p bs) =
  Fields.FieldLine
    <$> toPosition p
    <*> pure bs

-- | Attaches comments to a section argument. Note that section arguments
-- cannot actually have comments attached. That's because section arguments
-- must be on -- the same line as the section name, so all comments will end up
-- attached to the name.
sectionArg ::
  (Ord p) =>
  Fields.SectionArg p ->
  StateT.State [Comment.Comment p] (Fields.SectionArg (p, [Comment.Comment p]))
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
  (Ord p) =>
  p ->
  StateT.State [Comment.Comment p] (p, [Comment.Comment p])
toPosition p = do
  cs <- StateT.get
  let (xs, ys) = span ((<= p) . Comment.annotation) cs
  StateT.put ys
  pure (p, xs)
