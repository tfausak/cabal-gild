module CabalGild.Extra.List where

compareLength :: [a] -> Int -> Ordering
compareLength xs n =
  if n < 0
    then GT
    else case xs of
      [] -> compare 0 n
      _ : ys -> compareLength ys $ n - 1
