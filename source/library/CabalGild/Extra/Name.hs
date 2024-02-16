module CabalGild.Extra.Name where

import qualified Distribution.Fields as Fields

-- | Extracts the annotation from the given 'Fields.Name'.
annotation :: Fields.Name a -> a
annotation (Fields.Name x _) = x

-- | Extracts the value from the given 'Fields.Name'.
value :: Fields.Name a -> Fields.FieldName
value (Fields.Name _ x) = x
