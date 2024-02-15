module CabalGild.Extra.Either where

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just
