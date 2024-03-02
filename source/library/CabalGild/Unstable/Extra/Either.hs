module CabalGild.Unstable.Extra.Either where

-- | Converts an 'Either' to a 'Maybe'.
hush :: Either x a -> Maybe a
hush = either (const Nothing) Just
