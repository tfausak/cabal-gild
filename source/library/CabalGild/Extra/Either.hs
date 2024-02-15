module CabalGild.Extra.Either where

import qualified Control.Exception as Exception

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just

unsafeFromRight :: (Exception.Exception e) => Either e a -> a
unsafeFromRight = either Exception.throw id
