module CabalGild.Type.VersionRange where

import qualified Distribution.Types.PkgconfigVersion as PkgconfigVersion
import qualified Distribution.Types.PkgconfigVersionRange as PkgconfigVersionRange
import qualified Distribution.Types.Version as Version
import qualified Distribution.Types.VersionRange as VersionRange

-- | This type exists to provide an 'Ord' instance for
-- 'VersionRange.VersionRange', which was added in @Cabal-syntax-3.10.1.0@.
data VersionRange a
  = Any
  | This a
  | Later a
  | Earlier a
  | Union (VersionRange a) (VersionRange a)
  | Intersect (VersionRange a) (VersionRange a)
  deriving (Eq, Ord, Show)

fromPkgconfigVersionRange ::
  PkgconfigVersionRange.PkgconfigVersionRange ->
  VersionRange PkgconfigVersion.PkgconfigVersion
fromPkgconfigVersionRange x = case x of
  PkgconfigVersionRange.PcAnyVersion -> Any
  PkgconfigVersionRange.PcThisVersion v -> This v
  PkgconfigVersionRange.PcLaterVersion v -> Later v
  PkgconfigVersionRange.PcEarlierVersion v -> Earlier v
  PkgconfigVersionRange.PcOrLaterVersion v -> Union (Later v) (This v)
  PkgconfigVersionRange.PcOrEarlierVersion v -> Union (Earlier v) (This v)
  PkgconfigVersionRange.PcUnionVersionRanges v w -> Union (fromPkgconfigVersionRange v) (fromPkgconfigVersionRange w)
  PkgconfigVersionRange.PcIntersectVersionRanges v w -> Intersect (fromPkgconfigVersionRange v) (fromPkgconfigVersionRange w)

fromVersionRange :: VersionRange.VersionRange -> VersionRange Version.Version
fromVersionRange =
  VersionRange.foldVersionRange
    Any
    This
    Later
    Earlier
    Union
    Intersect
