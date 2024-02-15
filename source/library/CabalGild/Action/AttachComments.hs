module CabalGild.Action.AttachComments where

import qualified CabalGild.Type.Comment as Comment
import qualified Control.Monad.Trans.State as StateT
import qualified Distribution.Fields as Fields

run ::
  (Applicative m, Ord p) =>
  ([Fields.Field p], [Comment.Comment p]) ->
  m ([Fields.Field (p, [Comment.Comment p])], [Comment.Comment p])
run (fs, cs) = pure $ StateT.runState (fields fs) cs

fields ::
  (Ord p) =>
  [Fields.Field p] ->
  StateT.State [Comment.Comment p] [Fields.Field (p, [Comment.Comment p])]
fields = traverse field

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

name ::
  (Ord p) =>
  Fields.Name p ->
  StateT.State [Comment.Comment p] (Fields.Name (p, [Comment.Comment p]))
name (Fields.Name p fn) =
  Fields.Name
    <$> toPosition p
    <*> pure fn

fieldLine ::
  (Ord p) =>
  Fields.FieldLine p ->
  StateT.State [Comment.Comment p] (Fields.FieldLine (p, [Comment.Comment p]))
fieldLine (Fields.FieldLine p bs) =
  Fields.FieldLine
    <$> toPosition p
    <*> pure bs

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

toPosition ::
  (Ord p) =>
  p ->
  StateT.State [Comment.Comment p] (p, [Comment.Comment p])
toPosition p = do
  cs <- StateT.get
  let (xs, ys) = span ((<= p) . Comment.annotation) cs
  StateT.put ys
  pure (p, xs)
