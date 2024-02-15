module CabalGild.Action.RemovePositions where

import qualified CabalGild.Type.Comment as Comment
import qualified Distribution.Fields as Fields

run ::
  (Applicative m) =>
  ([Fields.Field (p, [Comment.Comment p])], [Comment.Comment p]) ->
  m ([Fields.Field [Comment.Comment ()]], [Comment.Comment ()])
run (fs, cs) = pure (fields fs, comments cs)

fields ::
  [Fields.Field (p, [Comment.Comment p])] ->
  [Fields.Field [Comment.Comment ()]]
fields = fmap field

field ::
  Fields.Field (p, [Comment.Comment p]) ->
  Fields.Field [Comment.Comment ()]
field f = case f of
  Fields.Field n fls -> Fields.Field (name n) $ fieldLines fls
  Fields.Section n sas fs -> Fields.Section (name n) (sectionArgs sas) $ fields fs

name ::
  Fields.Name (p, [Comment.Comment p]) ->
  Fields.Name [Comment.Comment ()]
name (Fields.Name (_, cs) x) = Fields.Name (comments cs) x

fieldLines ::
  [Fields.FieldLine (p, [Comment.Comment p])] ->
  [Fields.FieldLine [Comment.Comment ()]]
fieldLines = fmap fieldLine

fieldLine ::
  Fields.FieldLine (p, [Comment.Comment p]) ->
  Fields.FieldLine [Comment.Comment ()]
fieldLine (Fields.FieldLine (_, cs) x) = Fields.FieldLine (comments cs) x

sectionArgs ::
  [Fields.SectionArg (p, [Comment.Comment p])] ->
  [Fields.SectionArg [Comment.Comment ()]]
sectionArgs = fmap sectionArg

sectionArg ::
  Fields.SectionArg (p, [Comment.Comment p]) ->
  Fields.SectionArg [Comment.Comment ()]
sectionArg sa = case sa of
  Fields.SecArgName (_, cs) x -> Fields.SecArgName (comments cs) x
  Fields.SecArgStr (_, cs) x -> Fields.SecArgStr (comments cs) x
  Fields.SecArgOther (_, cs) x -> Fields.SecArgOther (comments cs) x

comments ::
  [Comment.Comment p] ->
  [Comment.Comment ()]
comments = fmap comment

comment ::
  Comment.Comment p ->
  Comment.Comment ()
comment c = c {Comment.annotation = ()}
