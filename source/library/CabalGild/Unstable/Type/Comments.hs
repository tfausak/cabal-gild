-- | This module defines the 'Comments' newtype wrapper.
module CabalGild.Unstable.Type.Comments where

import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified Distribution.Compat.Lens as Lens

data Comments p = MkComments
  { before :: [Comment.Comment p],
    after :: [Comment.Comment p]
  }
  deriving (Eq, Show)

instance Semigroup (Comments p) where
  xs <> ys =
    MkComments
      { before = before xs <> before ys,
        after = after ys <> after xs
      }

instance Monoid (Comments p) where
  mempty = empty

toList :: Comments p -> [Comment.Comment p]
toList x = before x <> after x

empty :: Comments p
empty = MkComments {before = [], after = []}

afterLens :: Lens.Lens' (Comments p) [Comment.Comment p]
afterLens f s = fmap (\x -> s {after = x}) . f $ after s
