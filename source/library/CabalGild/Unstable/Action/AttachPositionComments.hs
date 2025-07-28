-- | Specialized version of AttachComments that handles Position.Position
-- and properly distinguishes indented comments from top-level ones.
module CabalGild.Unstable.Action.AttachPositionComments where

import qualified CabalGild.Unstable.Action.AttachComments as AttachComments
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified Control.Monad.Trans.State as StateT
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec.Position as Position

-- | Position-aware version of AttachComments.run that properly handles
-- indented comments
run ::
  (Applicative m) =>
  ([Fields.Field Position.Position], [Comment.Comment Position.Position]) ->
  m ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], [Comment.Comment Position.Position])
run (fs, cs) = pure $ runWithPositionLogic fs cs

-- | Core logic that handles Position.Position specifically
runWithPositionLogic ::
  [Fields.Field Position.Position] ->
  [Comment.Comment Position.Position] ->
  ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], [Comment.Comment Position.Position])
runWithPositionLogic fs cs = 
  let (processedFields, remainingComments) = StateT.runState (traverse AttachComments.field fs) cs
      (indentedComments, topLevelComments) = partitionCommentsByIndentation remainingComments
  in case (indentedComments, reverse processedFields) of
    ([], _) -> (processedFields, topLevelComments)
    (_, []) -> (processedFields, remainingComments)  -- Keep all comments if no fields
    (indented, lastField : restFields) ->
      -- Attach indented comments to the last field processed
      let lastField' = attachCommentsToLastField indented lastField
          newProcessedFields = reverse (lastField' : restFields)
      in (newProcessedFields, topLevelComments)

-- | Partition comments based on their column position
-- Column 1 = top level, Column > 1 = indented (should stay with section/field)
partitionCommentsByIndentation :: 
  [Comment.Comment Position.Position] -> 
  ([Comment.Comment Position.Position], [Comment.Comment Position.Position])
partitionCommentsByIndentation = foldr classifyComment ([], [])
  where
    classifyComment comment (indented, topLevel) =
      let Position.Position _ col = Comment.annotation comment
      in if col > 1
         then (comment : indented, topLevel)    -- Indented comment - keep with field
         else (indented, comment : topLevel)     -- Top-level comment - separate

-- | Attach comments to the last field, handling both Fields and Sections recursively
attachCommentsToLastField :: 
  [Comment.Comment Position.Position] -> 
  Fields.Field (Position.Position, [Comment.Comment Position.Position]) -> 
  Fields.Field (Position.Position, [Comment.Comment Position.Position])
attachCommentsToLastField newComments f = case f of
  Fields.Field n fls -> case reverse fls of
    [] -> 
      -- No field lines, attach to field name
      let Fields.Name (pos, existingComments) fn = n
          newName = Fields.Name (pos, existingComments ++ newComments) fn
      in Fields.Field newName fls
    (lastFieldLine : restFieldLines) ->
      -- Attach to the last field line
      let Fields.FieldLine (pos, existingComments) bs = lastFieldLine
          newLastFieldLine = Fields.FieldLine (pos, existingComments ++ newComments) bs
          newFieldLines = reverse (newLastFieldLine : restFieldLines)
      in Fields.Field n newFieldLines
  Fields.Section n sas fs -> case reverse fs of
    [] -> 
      -- No fields in section, attach to section name
      let Fields.Name (pos, existingComments) fn = n
          newName = Fields.Name (pos, existingComments ++ newComments) fn
      in Fields.Section newName sas fs
    (lastField : restFields) ->
      -- Recursively attach to the last field within the section
      let lastField' = attachCommentsToLastField newComments lastField
      in Fields.Section n sas (reverse (lastField' : restFields))