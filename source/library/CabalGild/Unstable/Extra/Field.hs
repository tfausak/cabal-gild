module CabalGild.Unstable.Extra.Field where

import qualified Distribution.Fields as Fields

-- | Extracts the name of a 'Fields.Field'.
name :: Fields.Field a -> Fields.Name a
name f = case f of
  Fields.Field n _ -> n
  Fields.Section n _ _ -> n
