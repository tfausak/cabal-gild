module CabalGild.Unstable.Type.Optional where

-- | Represents a value that may or may not be present. Isomorphic to 'Maybe'.
-- Useful to distinguish between a value not being given and a value happening
-- to be the same as the default.
data Optional a
  = Default
  | Specific a
  deriving (Eq, Show)

-- | Converts a 'Maybe' value into an 'Optional' one.
fromMaybe :: Maybe a -> Optional a
fromMaybe = maybe Default Specific

-- | Converts an 'Optional' value into a 'Maybe' one.
toMaybe :: Optional a -> Maybe a
toMaybe o = case o of
  Default -> Nothing
  Specific s -> Just s
