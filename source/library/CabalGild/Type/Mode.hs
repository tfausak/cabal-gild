-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Type.Mode where

data Mode
  = Stdout
  | Inplace
  | Check
  deriving (Eq, Show)
