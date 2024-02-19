module CabalGild.Action.StripBlanks where

import qualified Data.ByteString.Char8 as Latin1
import qualified Distribution.Fields as Fields

-- | High level wrapper around 'field' that makes this action easier to compose
-- with other actions.
run ::
  (Applicative m) =>
  ([Fields.Field a], cs) ->
  m ([Fields.Field a], cs)
run (fs, cs) = pure (fmap field fs, cs)

-- | Strips blank space from the field recursively. In practice there should
-- not be any leading or trailing blank space to begin with. However the legacy
-- curly bracket syntax can introduce trailing blank space. For example with
-- @s { f : x }@ the field value will have a trailing space (@"x "@).
field :: Fields.Field a -> Fields.Field a
field f = case f of
  Fields.Field n fls ->
    Fields.Field (name n) (fmap fieldLine fls)
  Fields.Section n sas fs ->
    Fields.Section (name n) (fmap sectionArg sas) (fmap field fs)

-- | Strips blank space from the field's name.
name :: Fields.Name a -> Fields.Name a
name (Fields.Name a bs) = Fields.Name a (Latin1.strip bs)

-- | Strips blank space from the field line.
fieldLine :: Fields.FieldLine a -> Fields.FieldLine a
fieldLine (Fields.FieldLine a bs) = Fields.FieldLine a (Latin1.strip bs)

-- | Strips blank space from the section argument.
sectionArg :: Fields.SectionArg a -> Fields.SectionArg a
sectionArg sa = case sa of
  Fields.SecArgName a bs ->
    Fields.SecArgName a (Latin1.strip bs)
  Fields.SecArgStr a bs ->
    Fields.SecArgStr a (Latin1.strip bs)
  Fields.SecArgOther a bs ->
    Fields.SecArgOther a (Latin1.strip bs)
