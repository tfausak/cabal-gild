module CabalGild.Unstable.Extra.Name where

import qualified CabalGild.Unstable.Extra.String as String
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
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

-- | Returns true when the name is @"if"@, false otherwise.
isIf :: Fields.Name a -> Bool
isIf = (== String.toUtf8 "if") . value

-- | Returns true when the name is @"elif"@, false otherwise.
isElif :: CabalSpecVersion.CabalSpecVersion -> Fields.Name a -> Bool
isElif csv n =
  csv >= CabalSpecVersion.CabalSpecV2_2
    && value n == String.toUtf8 "elif"

-- | Returns true when the name is @"else"@, false otherwise.
isElse :: Fields.Name a -> Bool
isElse = (== String.toUtf8 "else") . value
