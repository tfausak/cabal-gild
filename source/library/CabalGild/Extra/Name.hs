module CabalGild.Extra.Name where

import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Fields as Fields

-- | Extracts the annotation from the given 'Fields.Name'.
annotation :: Fields.Name a -> a
annotation (Fields.Name x _) = x

-- | A lens for the 'annotation'.
annotationLens :: Lens.Lens' (Fields.Name a) a
annotationLens f s = fmap (\a -> Fields.Name a $ value s) . f $ annotation s

-- | Extracts the value from the given 'Fields.Name'.
value :: Fields.Name a -> Fields.FieldName
value (Fields.Name _ x) = x
