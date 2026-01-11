{-# LANGUAGE TypeOperators #-}

-- | This module attaches comments to fields and sections.
--
-- Comments are attached using structural analysis of the AST rather than
-- heuristics with magic numbers. The key principles are:
--
-- 1. **Blank lines are explicit boundaries**: A comment separated from an
--    element by a blank line is not a trailing comment of that element.
--
-- 2. **Indentation is derived, not hardcoded**: The expected indentation for
--    trailing comments is derived from the actual content of each scope,
--    not from hardcoded column numbers.
--
-- 3. **Scope is determined by AST structure**: The parser already knows
--    which elements belong to which scopes; we use that information.
module CabalGild.Unstable.Action.AttachComments
  ( run,
  )
where

import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.BlankLines as BlankLines
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.ScopeInfo as ScopeInfo
import qualified Control.Monad.Trans.State as StateT
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec.Position as Position

-- | High level wrapper around 'field' that makes this action easier to compose
-- with other actions.
--
-- This version takes the raw input to detect blank lines, which serve as
-- explicit boundaries for comment attachment.
run ::
  (Applicative m, p ~ Position.Position) =>
  ByteString.ByteString ->
  ([Fields.Field p], [Comment.Comment p]) ->
  m ([Fields.Field (p, Comments.Comments p)], [Comment.Comment p])
run input (fs, cs) =
  let blankLines = BlankLines.fromByteString input
   in pure $ StateT.runState (traverse (field blankLines) fs) cs

-- | Attaches comments to a single field.
--
-- It is assumed that both the fields and comments are already sorted by their
-- position @p@. This precondition is not checked.
--
-- Comments end up attached to the field's name or field lines. That's because
-- the 'Fields.Field' type doesn't have any annotations directly on it.
field ::
  (p ~ Position.Position) =>
  BlankLines.BlankLines ->
  Fields.Field p ->
  StateT.State [Comment.Comment p] (Fields.Field (p, Comments.Comments p))
field blankLines f = case f of
  Fields.Field n fls -> do
    n' <- name n
    fls' <- traverse fieldLine fls
    -- Build scope info for trailing comment attachment
    let scope = buildFieldScope n fls
    fls'' <- attachTrailingToLastFieldLine blankLines scope fls'
    pure $ Fields.Field n' fls''
  Fields.Section n sas fs -> do
    n' <- name n
    sas' <- traverse sectionArg sas
    fs' <- traverse (field blankLines) fs
    -- Build scope info for trailing comment attachment
    let scope = buildSectionScope n fs
    n'' <- attachTrailingToSectionName blankLines scope n'
    pure $ Fields.Section n'' sas' fs'

-- | Build scope information for a field from its structure.
buildFieldScope ::
  (p ~ Position.Position) =>
  Fields.Name p ->
  [Fields.FieldLine p] ->
  ScopeInfo.ScopeInfo
buildFieldScope n fls =
  let namePos = Name.annotation n
      firstLinePos = FieldLine.annotation <$> Maybe.listToMaybe fls
      lastLinePos = FieldLine.annotation <$> Maybe.listToMaybe (reverse fls)
   in ScopeInfo.fieldScope namePos lastLinePos firstLinePos

-- | Build scope information for a section from its structure.
buildSectionScope ::
  (p ~ Position.Position) =>
  Fields.Name p ->
  [Fields.Field p] ->
  ScopeInfo.ScopeInfo
buildSectionScope n fs =
  let namePos = Name.annotation n
      -- Find the end row from the last nested element
      endRow = case reverse fs of
        [] -> Position.positionRow namePos
        (lastField : _) -> fieldEndRow lastField
      -- Find content column from first nested element
      contentCol = case fs of
        [] -> Nothing
        (firstField : _) -> Just $ fieldStartCol firstField
   in ScopeInfo.sectionScope namePos endRow contentCol

-- | Get the last row of a field (including all its content).
fieldEndRow :: (p ~ Position.Position) => Fields.Field p -> Int
fieldEndRow f = case f of
  Fields.Field n fls ->
    case reverse fls of
      [] -> Position.positionRow $ Name.annotation n
      (lastFl : _) -> Position.positionRow $ FieldLine.annotation lastFl
  Fields.Section n _ fs ->
    case reverse fs of
      [] -> Position.positionRow $ Name.annotation n
      (lastField : _) -> fieldEndRow lastField

-- | Get the starting column of a field.
fieldStartCol :: (p ~ Position.Position) => Fields.Field p -> Int
fieldStartCol f = case f of
  Fields.Field n _ -> Position.positionCol $ Name.annotation n
  Fields.Section n _ _ -> Position.positionCol $ Name.annotation n

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

-- | Attaches comments to a section argument.
--
-- Note that section arguments cannot actually have comments attached. That's
-- because section arguments must be on the same line as the section name, so
-- all comments will end up attached to the name.
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
--
-- Comments are attached when their position is less than or equal to the given
-- position. The comments are removed from the state as they are attached.
-- These become "before" comments (they appear before the element in the output).
toPosition ::
  (Ord p) =>
  p ->
  StateT.State [Comment.Comment p] (p, Comments.Comments p)
toPosition p = do
  cs <- StateT.get
  let (xs, ys) = span ((<= p) . Comment.annotation) cs
  StateT.put ys
  pure (p, Comments.onlyBefore xs)

-- | Check if a comment should be attached as a trailing comment.
--
-- A comment is a trailing comment if:
-- 1. It appears after the element (on a later row)
-- 2. It is within the element's scope (not past the scope's end row)
-- 3. There is no blank line between the element and the comment
-- 4. It is properly indented for the scope
shouldAttachTrailing ::
  (p ~ Position.Position) =>
  BlankLines.BlankLines ->
  ScopeInfo.ScopeInfo ->
  -- | Position of the element the comment might attach to
  Position.Position ->
  Comment.Comment p ->
  Bool
shouldAttachTrailing blankLines scope elementPos comment =
  let commentPos = Comment.annotation comment
      commentRow = Position.positionRow commentPos
      commentCol = Position.positionCol commentPos
      elementRow = Position.positionRow elementPos
      -- Comment must be on a later row
      afterElement = commentRow > elementRow
      -- Comment must be within the scope's boundaries
      withinScope = ScopeInfo.containsRow scope commentRow
      -- No blank line between element and comment
      noBlankBetween = not $ BlankLines.hasBlankBetween blankLines elementRow commentRow
      -- Comment must be properly indented for this scope
      properlyIndented = ScopeInfo.isProperlyIndented scope commentCol
   in afterElement && withinScope && noBlankBetween && properlyIndented

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
--
-- If the list is empty, return it unchanged.
attachTrailingToLastFieldLine ::
  (p ~ Position.Position) =>
  BlankLines.BlankLines ->
  ScopeInfo.ScopeInfo ->
  [Fields.FieldLine (p, Comments.Comments p)] ->
  StateT.State
    [Comment.Comment p]
    [Fields.FieldLine (p, Comments.Comments p)]
attachTrailingToLastFieldLine blankLines scope fls = case reverse fls of
  [] -> pure []
  (Fields.FieldLine (pos, comments) bs : rest) -> do
    trailingComments <-
      takeTrailingComments (shouldAttachTrailing blankLines scope pos)
    let lastFl =
          Fields.FieldLine
            (pos, comments <> Comments.onlyAfter trailingComments)
            bs
    pure $ reverse (lastFl : rest)

-- | Attach trailing comments to a section name.
attachTrailingToSectionName ::
  (p ~ Position.Position) =>
  BlankLines.BlankLines ->
  ScopeInfo.ScopeInfo ->
  Fields.Name (p, Comments.Comments p) ->
  StateT.State
    [Comment.Comment p]
    (Fields.Name (p, Comments.Comments p))
attachTrailingToSectionName blankLines scope (Fields.Name (pos, comments) fn) = do
  trailingComments <-
    takeTrailingComments (shouldAttachTrailing blankLines scope pos)
  pure $ Fields.Name (pos, comments <> Comments.onlyAfter trailingComments) fn
