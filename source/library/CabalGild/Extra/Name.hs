module CabalGild.Extra.Name where

import qualified Distribution.Fields as Fields

annotation :: Fields.Name a -> a
annotation (Fields.Name x _) = x

value :: Fields.Name a -> Fields.FieldName
value (Fields.Name _ x) = x
