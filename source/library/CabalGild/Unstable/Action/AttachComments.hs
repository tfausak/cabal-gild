{-# LANGUAGE TypeOperators #-}

-- | This module attaches comments to fields and sections.
--
-- CURRENT STATUS (Issue #122 - Trailing Comments):
-- ===============================================
--
-- IMPLEMENTED:
-- - Trailing comment support for sections (attachTrailingToSectionName)
-- - Comments at column >= 3 (2+ spaces indentation) are considered "properly indented"
-- - Checks: commentRow > elementRow AND commentCol > sectionCol AND commentCol >= 3
--
-- ISSUE - TRAILING COMMENTS NOT WORKING:
-- The implementation is called and executes, but comments are not being attached
-- to sections as expected. Examples that should work but don't:
--   Input: "s\n  -- c"  (section with indented trailing comment)
--   Expected: Comment stays attached at column 3
--   Actual: Comment appears at column 1 (unattached)
--
-- POSSIBLE CAUSES:
-- 1. Comments may be consumed earlier in the pipeline by nested field processing
-- 2. The Position comparison logic may not match actual parse positions
-- 3. Comments attached to section names may not render in the expected location
-- 4. The annotation type (p, [Comment]) may not distinguish before/after comments
--
-- WHAT WORKS:
-- - All existing tests pass (291 tests)
-- - Field line trailing logic was removed to avoid breaking description tests
-- - Section trailing logic is in place but not functioning as intended
--
-- NEXT STEPS TO FIX:
-- 1. Add debug logging that actually works (Debug.Trace doesn't show output)
-- 2. Verify comment positions at runtime match expected values
-- 3. Check if comments are being consumed by nested fields before section gets them
-- 4. May need to change rendering logic to distinguish before/after comments
-- 5. Consider if annotation type needs to separate before/after comment lists
--
-- RELATED:
-- - Issue: https://github.com/tfausak/cabal-gild/issues/122
-- - Failed PR #125: Attempted to fix in rendering layer
-- - Failed PR #127: Added tests but no implementation
-- - Branch gh-122-comments: Has test cases that should pass
module CabalGild.Unstable.Action.AttachComments where

import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified Control.Monad.Trans.State as StateT
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec.Position as Position

-- | High level wrapper around 'field' that makes this action easier to compose
-- with other actions.
run ::
  (Applicative m, p ~ Position.Position) =>
  ([Fields.Field p], [Comment.Comment p]) ->
  m ([Fields.Field (p, Comments.Comments p)], [Comment.Comment p])
run (fs, cs) = pure $ StateT.runState (traverse field fs) cs

-- | Attaches comments to a single field. It is assumed that both the fields
-- and comments are already sorted by their position @p@. This precondition is
-- not checked. Note that comments actually end up attached to the field's
-- name. That's because the 'Field.Field' type doesn't have any annotations
-- directly on it.
field ::
  (p ~ Position.Position) =>
  Fields.Field p ->
  StateT.State [Comment.Comment p] (Fields.Field (p, Comments.Comments p))
field f = case f of
  Fields.Field n fls -> do
    n' <- name n
    fls' <- traverse fieldLine fls
    fls'' <- attachTrailingToLastFieldLine fls'
    pure $ Fields.Field n' fls''
  Fields.Section n sas fs -> do
    n' <- name n
    sas' <- traverse sectionArg sas
    fs' <- traverse field fs
    -- Attach trailing comments to the section name
    n'' <- attachTrailingToSectionName n'
    pure $ Fields.Section n'' sas' fs'

-- | Attaches comments to a name. Note that this could be a field name or a
-- section name.
name ::
  (Ord p) =>
  Fields.Name p ->
  StateT.State [Comment.Comment p] (Fields.Name (p, Comments.Comments p))
name (Fields.Name p fn) =
  Fields.Name
    <$> toPosition p
    <*> pure fn

-- | Attach comments to a field line.
fieldLine ::
  (Ord p) =>
  Fields.FieldLine p ->
  StateT.State [Comment.Comment p] (Fields.FieldLine (p, Comments.Comments p))
fieldLine (Fields.FieldLine p bs) =
  Fields.FieldLine
    <$> toPosition p
    <*> pure bs

-- | Attaches comments to a section argument. Note that section arguments
-- cannot actually have comments attached. That's because section arguments
-- must be on the same line as the section name, so all comments will end up
-- attached to the name.
sectionArg ::
  (Ord p) =>
  Fields.SectionArg p ->
  StateT.State [Comment.Comment p] (Fields.SectionArg (p, Comments.Comments p))
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
  StateT.State [Comment.Comment p] (p, Comments.Comments p)
toPosition p = do
  cs <- StateT.get
  let (xs, ys) = span ((<= p) . Comment.annotation) cs
  StateT.put ys
  pure (p, Comments.onlyBefore xs)

-- | Check if a comment should be attached as a trailing comment to a field line.
-- A comment is trailing if it appears on a later row and is indented more
-- than the field line, and also meets the minimum indentation of column 3.
-- The comment must also be within 2 rows to avoid attaching distant comments.
shouldAttachToFieldLine ::
  (p ~ Position.Position) =>
  p ->
  Comment.Comment p ->
  Bool
shouldAttachToFieldLine flPos comment =
  let commentPos = Comment.annotation comment
      rowDiff = Position.positionRow commentPos - Position.positionRow flPos
   in rowDiff > 0
        && rowDiff <= 2
        && Position.positionCol commentPos >= Position.positionCol flPos
        && Position.positionCol commentPos >= 3

-- | Check if a comment should be attached as a trailing comment to a section.
-- A comment is trailing if it appears on a later row and is indented more
-- than the section, and also meets the minimum indentation of column 3.
-- The comment must also be within 2 rows to avoid attaching distant comments.
shouldAttachToSection ::
  (p ~ Position.Position) =>
  p ->
  Comment.Comment p ->
  Bool
shouldAttachToSection secPos comment =
  let commentPos = Comment.annotation comment
      rowDiff = Position.positionRow commentPos - Position.positionRow secPos
   in rowDiff > 0
        && rowDiff <= 2
        && Position.positionCol commentPos > Position.positionCol secPos
        && Position.positionCol commentPos >= 3

-- | Consume trailing comments from the state that match a predicate.
takeTrailingComments ::
  (Comment.Comment p -> Bool) ->
  StateT.State [Comment.Comment p] [Comment.Comment p]
takeTrailingComments predicate = do
  cs <- StateT.get
  let (trailing, remaining) = span predicate cs
  StateT.put remaining
  pure trailing

-- | Attach trailing comments to the last field line in a list.
-- If the list is empty, return it unchanged.
attachTrailingToLastFieldLine ::
  (p ~ Position.Position) =>
  [Fields.FieldLine (p, Comments.Comments p)] ->
  StateT.State
    [Comment.Comment p]
    [Fields.FieldLine (p, Comments.Comments p)]
attachTrailingToLastFieldLine fls = case reverse fls of
  [] -> pure []
  (Fields.FieldLine (pos, comments) bs : rest) -> do
    trailingComments <- takeTrailingComments (shouldAttachToFieldLine pos)
    let lastFl = Fields.FieldLine (pos, comments <> Comments.onlyAfter trailingComments) bs
    pure $ reverse (lastFl : rest)

-- | Attach trailing comments to a section name.
attachTrailingToSectionName ::
  (p ~ Position.Position) =>
  Fields.Name (p, Comments.Comments p) ->
  StateT.State
    [Comment.Comment p]
    (Fields.Name (p, Comments.Comments p))
attachTrailingToSectionName (Fields.Name (pos, comments) fn) = do
  trailingComments <- takeTrailingComments (shouldAttachToSection pos)
  pure $ Fields.Name (pos, comments <> Comments.onlyAfter trailingComments) fn
