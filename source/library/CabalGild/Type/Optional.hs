module CabalGild.Type.Optional where

data Optional a
  = Default
  | Specific a
  deriving (Eq, Show)

withDefault :: a -> Optional a -> a
withDefault = flip optional id

optional :: b -> (a -> b) -> Optional a -> b
optional b f o = case o of
  Default -> b
  Specific s -> f s
