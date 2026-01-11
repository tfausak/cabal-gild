-- | This module defines types for structure-aware comment attachment.
--
-- Instead of using magic numbers (like "within 2 rows" or "column >= 3"),
-- we derive scope information from the actual AST structure. This makes
-- comment attachment robust and adaptable to different formatting styles.
module CabalGild.Unstable.Type.ScopeInfo
  ( -- * Types
    ScopeInfo (..),
    ScopeType (..),

    -- * Smart constructors
    fieldScope,
    sectionScope,

    -- * Queries
    containsRow,
    isProperlyIndented,
  )
where

import qualified Distribution.Parsec.Position as Position

-- | Information about a scope (field or section) derived from the AST.
--
-- This eliminates the need for magic numbers by capturing the actual
-- structure of each element.
data ScopeInfo = ScopeInfo
  { -- | The row where this scope's header begins.
    scopeStartRow :: !Int,
    -- | The last row that belongs to this scope (inclusive).
    -- For a field, this is the row of the last field line.
    -- For a section, this is the last row of its last child.
    scopeEndRow :: !Int,
    -- | The column where the scope's header (name) starts.
    -- This is the reference indentation for the scope itself.
    scopeHeaderCol :: !Int,
    -- | The expected indentation for content within this scope.
    -- Derived from the first actual content element, not hardcoded.
    -- 'Nothing' means the scope has no content (empty field/section).
    scopeContentCol :: !(Maybe Int),
    -- | The type of scope for context-specific attachment rules.
    scopeType :: !ScopeType
  }
  deriving (Eq, Show)

-- | The type of scope, which affects comment attachment semantics.
data ScopeType
  = -- | A field scope (e.g., "exposed-modules:", "build-depends:")
    FieldScope
  | -- | A section scope (e.g., "library", "executable foo")
    SectionScope
  deriving (Eq, Show)

-- | Create a 'ScopeInfo' for a field.
--
-- The content column is derived from the first field line, if any.
fieldScope ::
  -- | Position of the field name
  Position.Position ->
  -- | Position of the last field line (if any)
  Maybe Position.Position ->
  -- | Position of the first field line (for content column)
  Maybe Position.Position ->
  ScopeInfo
fieldScope namePos lastLinePos firstLinePos =
  ScopeInfo
    { scopeStartRow = Position.positionRow namePos,
      scopeEndRow = maybe (Position.positionRow namePos) Position.positionRow lastLinePos,
      scopeHeaderCol = Position.positionCol namePos,
      scopeContentCol = Position.positionCol <$> firstLinePos,
      scopeType = FieldScope
    }

-- | Create a 'ScopeInfo' for a section.
--
-- The content column is derived from the first child element, if any.
sectionScope ::
  -- | Position of the section name
  Position.Position ->
  -- | End row of the section (last row of last child, or section row if empty)
  Int ->
  -- | Column of first child element (for content column)
  Maybe Int ->
  ScopeInfo
sectionScope namePos endRow contentCol =
  ScopeInfo
    { scopeStartRow = Position.positionRow namePos,
      scopeEndRow = endRow,
      scopeHeaderCol = Position.positionCol namePos,
      scopeContentCol = contentCol,
      scopeType = SectionScope
    }

-- | Check if a row is adjacent to this scope for trailing comment attachment.
--
-- A trailing comment must be:
-- 1. After the scope's header row
-- 2. At most 1 row after the scope's end row (immediately following the content)
--
-- This prevents comments from attaching to distant elements. For example:
--
-- @
-- f: 1       -- scopeEndRow = 1 for f
-- g: 2
--    -- c    -- row 3, should NOT attach to f (3 > 1+1)
-- @
--
-- Without this check, the comment could incorrectly attach to @f@ because
-- processing happens in order and @f@ would consume the comment before
-- @g@ is even considered.
containsRow :: ScopeInfo -> Int -> Bool
containsRow scope row =
  row > scopeStartRow scope && row <= scopeEndRow scope + 1

-- | Check if a comment at the given column is properly indented for this scope.
--
-- For trailing comments, we require the comment to be at least as indented
-- as the actual content. This handles several cases correctly:
--
-- 1. Inline field values (e.g., "cabal-version: 3.0" where 3.0 is at col 16):
--    A comment at col 3 is NOT properly indented (3 < 16), so it floats out.
--
-- 2. Properly indented content (e.g., "f:\n  1" where 1 is at col 3):
--    A comment at col 3 IS properly indented (3 >= 3), so it attaches.
--
-- 3. Improperly indented content (e.g., "f:\n 1" where 1 is at col 2):
--    A comment at col 2 would attach, but cabal-gild normalizes indentation,
--    so we also require the minimum normalized column (headerCol + 2).
--
-- If there's no content yet (empty field/section), the comment must be
-- more indented than the header.
isProperlyIndented :: ScopeInfo -> Int -> Bool
isProperlyIndented scope commentCol =
  let minNormalizedCol = scopeHeaderCol scope + 2
   in case scopeContentCol scope of
        -- Content exists: comment must be >= content column AND >= normalized minimum
        Just contentCol -> commentCol >= contentCol && commentCol >= minNormalizedCol
        -- No content: comment must be >= normalized minimum
        Nothing -> commentCol >= minNormalizedCol
