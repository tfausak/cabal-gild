module CabalGild.Unstable.Exception.UnsatisfiedRequire where

import qualified CabalGild.Unstable.Type.VersionRange as VersionRange
import qualified Control.Exception as Exception
import qualified Data.Version as Version
import qualified Distribution.Pretty as Pretty

data UnsatisfiedRequire = UnsatisfiedRequire
  { actual :: Version.Version,
    range :: VersionRange.VersionRange
  }
  deriving (Eq, Show)

instance Exception.Exception UnsatisfiedRequire where
  displayException (UnsatisfiedRequire v r) =
    "cabal-gild version "
      <> Version.showVersion v
      <> " does not satisfy the required range "
      <> Pretty.prettyShow r
