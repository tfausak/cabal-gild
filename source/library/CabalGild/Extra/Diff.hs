module CabalGild.Extra.Diff where

import qualified Data.Algorithm.Diff as Diff

-- | Returns 'True' if the given 'Diff.PolyDiff' is 'Diff.Both', 'False'
-- otherwise.
isBoth :: Diff.PolyDiff a b -> Bool
isBoth x = case x of
  Diff.First _ -> False
  Diff.Second _ -> False
  Diff.Both _ _ -> True
