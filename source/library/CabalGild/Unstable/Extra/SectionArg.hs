module CabalGild.Unstable.Extra.SectionArg where

import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Fields as Fields

-- | Extracts the annotation from the given 'Fields.SectionArg'.
annotation :: Fields.SectionArg a -> a
annotation sa = case sa of
  Fields.SecArgName x _ -> x
  Fields.SecArgStr x _ -> x
  Fields.SecArgOther x _ -> x

-- | A lens for the 'annotation'.
annotationLens :: Lens.Lens (Fields.SectionArg a) (Fields.SectionArg b) a b
annotationLens f sa =
  fmap
    ( \a -> case sa of
        Fields.SecArgName _ x -> Fields.SecArgName a x
        Fields.SecArgStr _ x -> Fields.SecArgStr a x
        Fields.SecArgOther _ x -> Fields.SecArgOther a x
    )
    . f
    $ annotation sa

-- | Extracts the value from the given 'Fields.SectionArg'.
value :: Fields.SectionArg a -> ByteString.ByteString
value sa = case sa of
  Fields.SecArgName _ x -> x
  Fields.SecArgStr _ x -> x
  Fields.SecArgOther _ x -> x
