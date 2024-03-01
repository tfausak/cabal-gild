module CabalGild.Extra.ByteString where

import qualified Data.ByteString as ByteString

-- | Calling @replace needle replacement haystack@ will replace every
-- non-overlapping occurrence of @needle@ in @haystack@ with @replacement@.
--
-- <https://github.com/haskell/bytestring/issues/307>
replace ::
  ByteString.ByteString ->
  ByteString.ByteString ->
  ByteString.ByteString ->
  ByteString.ByteString
replace needle replacement = ByteString.intercalate replacement . splitOn needle

-- | Calling @splitOn needle haystack@ will split @haystack@ into a list of
-- substrings using @needle@ as the delimiter.
--
-- <https://github.com/haskell/bytestring/issues/100>
splitOn ::
  ByteString.ByteString ->
  ByteString.ByteString ->
  [ByteString.ByteString]
splitOn needle =
  let split = ByteString.breakSubstring needle
      go haystack =
        let (before, after) = split haystack
         in before
              : if ByteString.null after
                then []
                else go (ByteString.drop (ByteString.length needle) after)
   in go
