module CabalGild.Action.RemovePositions where

import qualified CabalGild.Type.Comment as Comment
import qualified Distribution.Fields as Fields

-- | A wrapper around 'fields' to allow this to be composed with other actions.
run ::
  (Applicative m) =>
  ([Fields.Field (p, [Comment.Comment p])], [Comment.Comment p]) ->
  m ([Fields.Field [Comment.Comment ()]], [Comment.Comment ()])
run (fs, cs) = pure (fields fs, comments cs)

-- | Removes the positions from some fields and their comments. This is useful
-- for two reasons: the annotations become simpler, and it's clear that the
-- positions won't be used for anything else.
fields ::
  [Fields.Field (p, [Comment.Comment p])] ->
  [Fields.Field [Comment.Comment ()]]
fields = fmap field

-- | Removes the positions from a field and its comments.
field ::
  Fields.Field (p, [Comment.Comment p]) ->
  Fields.Field [Comment.Comment ()]
field f = case f of
  Fields.Field n fls -> Fields.Field (name n) $ fieldLines fls
  Fields.Section n sas fs -> Fields.Section (name n) (sectionArgs sas) $ fields fs

-- | Removes the positions from a name and its comments.
name ::
  Fields.Name (p, [Comment.Comment p]) ->
  Fields.Name [Comment.Comment ()]
name (Fields.Name (_, cs) x) = Fields.Name (comments cs) x

-- | Removes the positions from field lines and their comments.
fieldLines ::
  [Fields.FieldLine (p, [Comment.Comment p])] ->
  [Fields.FieldLine [Comment.Comment ()]]
fieldLines = fmap fieldLine

-- | Removes the positions from a field line and its comments.
fieldLine ::
  Fields.FieldLine (p, [Comment.Comment p]) ->
  Fields.FieldLine [Comment.Comment ()]
fieldLine (Fields.FieldLine (_, cs) x) = Fields.FieldLine (comments cs) x

-- | Removes the positions from some section arguments and their comments.
sectionArgs ::
  [Fields.SectionArg (p, [Comment.Comment p])] ->
  [Fields.SectionArg [Comment.Comment ()]]
sectionArgs = fmap sectionArg

-- | Removes the positions from a section argument and its comments.
sectionArg ::
  Fields.SectionArg (p, [Comment.Comment p]) ->
  Fields.SectionArg [Comment.Comment ()]
sectionArg sa = case sa of
  Fields.SecArgName (_, cs) x -> Fields.SecArgName (comments cs) x
  Fields.SecArgStr (_, cs) x -> Fields.SecArgStr (comments cs) x
  Fields.SecArgOther (_, cs) x -> Fields.SecArgOther (comments cs) x

-- | Removes the positions from some comments.
comments ::
  [Comment.Comment p] ->
  [Comment.Comment ()]
comments = fmap comment

-- | Removes the position from a comment.
comment ::
  Comment.Comment p ->
  Comment.Comment ()
comment c = c {Comment.annotation = ()}
