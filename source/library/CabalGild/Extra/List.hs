module CabalGild.Extra.List where

-- | Compares the length of the given list to the given integer. This is
-- equivalent to @compare (length xs) n@, but it can be more efficient if @n@
-- is less than @length xs@.
compareLength :: [a] -> Int -> Ordering
compareLength xs n =
  if n < 0
    then GT
    else case xs of
      [] -> compare 0 n
      _ : ys -> compareLength ys $ n - 1
