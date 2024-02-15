module CabalGild.Extra.Field where

import qualified Distribution.Fields as Fields

name :: Fields.Field a -> Fields.Name a
name f = case f of
  Fields.Field n _ -> n
  Fields.Section n _ _ -> n
