module CabalGild.Unstable.Type.VersionRange where

import qualified CabalGild.Unstable.Type.VersionRange.Complex as Complex
import qualified CabalGild.Unstable.Type.VersionRange.Operator as Operator
import qualified CabalGild.Unstable.Type.VersionRange.Part as Part
import qualified CabalGild.Unstable.Type.VersionRange.Simple as Simple
import qualified CabalGild.Unstable.Type.VersionRange.Version as Version
import qualified CabalGild.Unstable.Type.VersionRange.Versions as Versions
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.Version as CabalVersion
import qualified Distribution.Types.VersionRange as CabalRange
import qualified Numeric.Natural as Natural

newtype VersionRange = VersionRange
  { unwrap :: Complex.Complex Simple.Simple
  }
  deriving (Eq, Ord, Show)

instance Parsec.Parsec VersionRange where
  parsec = VersionRange <$> Complex.parse Simple.parse

instance Pretty.Pretty VersionRange where
  pretty = Complex.render Simple.render . unwrap

-- | Converts a 'VersionRange' to Cabal-syntax's 'CabalRange.VersionRange' for
-- evaluation with 'CabalRange.withinRange'.
toCabalVersionRange :: VersionRange -> CabalRange.VersionRange
toCabalVersionRange = toCabalComplex . unwrap

toCabalComplex :: Complex.Complex Simple.Simple -> CabalRange.VersionRange
toCabalComplex x = case x of
  Complex.Par c -> toCabalComplex c
  Complex.And l r -> CabalRange.intersectVersionRanges (toCabalComplex l) (toCabalComplex r)
  Complex.Or l r -> CabalRange.unionVersionRanges (toCabalComplex l) (toCabalComplex r)
  Complex.Simple s -> toCabalSimple s

toCabalSimple :: Simple.Simple -> CabalRange.VersionRange
toCabalSimple x = case x of
  Simple.Any -> CabalRange.anyVersion
  Simple.None -> CabalRange.noVersion
  Simple.Op op vs -> toCabalOp op vs

toCabalOp :: Operator.Operator -> Versions.Versions -> CabalRange.VersionRange
toCabalOp op vs = case vs of
  Versions.One v -> toCabalOpOne op v
  Versions.Set s -> case Set.toList s of
    [] -> CabalRange.noVersion
    v : rest -> foldr (CabalRange.unionVersionRanges . toCabalOpOne op) (toCabalOpOne op v) rest

toCabalOpOne :: Operator.Operator -> Version.Version -> CabalRange.VersionRange
toCabalOpOne op v = case op of
  Operator.Caret -> CabalRange.majorBoundVersion cv
  Operator.Ge -> CabalRange.orLaterVersion cv
  Operator.Gt -> CabalRange.laterVersion cv
  Operator.Le -> CabalRange.orEarlierVersion cv
  Operator.Lt -> CabalRange.earlierVersion cv
  Operator.Eq
    | hasWildcard v -> CabalRange.withinVersion cv
    | otherwise -> CabalRange.thisVersion cv
  where
    cv = toCabalVersion v

hasWildcard :: Version.Version -> Bool
hasWildcard (Version.MkVersion parts) = any isWildcard $ NonEmpty.toList parts
  where
    isWildcard Part.Wildcard = True
    isWildcard _ = False

toCabalVersion :: Version.Version -> CabalVersion.Version
toCabalVersion (Version.MkVersion parts) =
  CabalVersion.mkVersion
    . fmap fromIntegral
    . concatMap toNumbers
    $ NonEmpty.toList parts
  where
    toNumbers :: Part.Part -> [Natural.Natural]
    toNumbers (Part.Numeric n) = [n]
    toNumbers Part.Wildcard = []
