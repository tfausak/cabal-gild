module CabalGild.Unstable.Type.DiscoverTarget where

-- | This type determines the type of thing to search for when using the
-- @discover@ pragma.
data DiscoverTarget
  = -- | Discovers any file at all.
    Files
  | -- | Discovers only Haskell modules, which also includes signatures and
    -- various pre-processors.
    Modules
  deriving (Eq, Show)
