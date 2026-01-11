-- | This module defines the 'Comments' data type which separates comments
-- that appear before an element from those that appear after.
module CabalGild.Unstable.Type.Comments where

import qualified CabalGild.Unstable.Type.Comment as Comment

-- | Comments that can appear before and/or after an element.
-- This separation is necessary for proper rendering of trailing comments.
data Comments a = Comments
  { before :: [Comment.Comment a],
    after :: [Comment.Comment a]
  }
  deriving (Eq, Show)

-- | Empty comments (no before, no after)
empty :: Comments a
empty = Comments [] []

-- | Check if there are no comments at all
isEmpty :: Comments a -> Bool
isEmpty cs = null (before cs) && null (after cs)

-- | Create Comments with only before comments
onlyBefore :: [Comment.Comment a] -> Comments a
onlyBefore cs = Comments cs []

-- | Create Comments with only after comments
onlyAfter :: [Comment.Comment a] -> Comments a
onlyAfter = Comments []

-- | Convert from a single list to Comments (all treated as before)
fromList :: [Comment.Comment a] -> Comments a
fromList cs = Comments cs []

-- | Combine all comments into a single list (before followed by after)
toList :: Comments a -> [Comment.Comment a]
toList cs = before cs <> after cs

-- | Combine two Comments, concatenating before and after separately
instance Semigroup (Comments a) where
  Comments b1 a1 <> Comments b2 a2 = Comments (b1 <> b2) (a1 <> a2)

instance Monoid (Comments a) where
  mempty = empty
