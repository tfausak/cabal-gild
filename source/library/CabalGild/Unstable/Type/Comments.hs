-- | This module defines the 'Comments' newtype wrapper.
module CabalGild.Unstable.Type.Comments where

import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified Data.Function as Function

newtype Comments p
  = MkComments [Comment.Comment p]
  deriving (Eq, Show)

instance Semigroup (Comments p) where
  xs <> ys = fromList $ Function.on (<>) toList xs ys

instance Monoid (Comments p) where
  mempty = empty

fromList :: [Comment.Comment p] -> Comments p
fromList = MkComments

toList :: Comments p -> [Comment.Comment p]
toList (MkComments xs) = xs

empty :: Comments p
empty = fromList []

isEmpty :: Comments p -> Bool
isEmpty = null . toList
