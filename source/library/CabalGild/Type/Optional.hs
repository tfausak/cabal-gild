module CabalGild.Type.Optional where

-- | Represents a value that may or may not be present. Isomorphic to 'Maybe'.
-- Useful to distinguish between a value not being given and a value happening
-- to be the same as the default.
data Optional a
  = Default
  | Specific a
  deriving (Eq, Show)

-- | Uses the provided default value if the optional is 'Default', otherwise
-- gets the value out of the 'Specific'. Similar to 'Data.Maybe.fromMaybe'.
withDefault :: a -> Optional a -> a
withDefault = flip optional id

-- | Basic destructor, similar to 'maybe'.
optional :: b -> (a -> b) -> Optional a -> b
optional b f o = case o of
  Default -> b
  Specific s -> f s
