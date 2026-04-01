{-# LANGUAGE FlexibleInstances #-}

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
import qualified Distribution.Types.VersionRange as CabalVR
import qualified Numeric.Natural as Natural

type VersionRange = Complex.Complex Simple.Simple

instance Parsec.Parsec VersionRange where
  parsec = Complex.parseComplex Simple.parseSimple

instance Pretty.Pretty VersionRange where
  pretty = Complex.renderComplex Simple.renderSimple

-- | Converts a 'VersionRange' to Cabal-syntax's 'CabalVR.VersionRange' for
-- evaluation with 'CabalVR.withinRange'.
toCabalVersionRange :: VersionRange -> CabalVR.VersionRange
toCabalVersionRange = toCabalComplex

toCabalComplex :: Complex.Complex Simple.Simple -> CabalVR.VersionRange
toCabalComplex x = case x of
  Complex.Par c -> toCabalComplex c
  Complex.And l r -> CabalVR.intersectVersionRanges (toCabalSimple l) (toCabalComplex r)
  Complex.Or l r -> CabalVR.unionVersionRanges (toCabalSimple l) (toCabalComplex r)
  Complex.Simple s -> toCabalSimple s

toCabalSimple :: Simple.Simple -> CabalVR.VersionRange
toCabalSimple x = case x of
  Simple.Any -> CabalVR.anyVersion
  Simple.None -> CabalVR.noVersion
  Simple.Op op vs -> toCabalOp op vs

toCabalOp :: Operator.Operator -> Versions.Versions -> CabalVR.VersionRange
toCabalOp op vs = case vs of
  Versions.One v -> toCabalOpOne op v
  Versions.Set s -> case Set.toList s of
    [] -> CabalVR.noVersion
    v : rest -> foldr (CabalVR.unionVersionRanges . toCabalOpOne op) (toCabalOpOne op v) rest

toCabalOpOne :: Operator.Operator -> Version.Version -> CabalVR.VersionRange
toCabalOpOne op v = case op of
  Operator.Caret -> CabalVR.majorBoundVersion cv
  Operator.Ge -> CabalVR.orLaterVersion cv
  Operator.Gt -> CabalVR.laterVersion cv
  Operator.Le -> CabalVR.orEarlierVersion cv
  Operator.Lt -> CabalVR.earlierVersion cv
  Operator.Eq
    | hasWildcard v -> CabalVR.withinVersion cv
    | otherwise -> CabalVR.thisVersion cv
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
    . map fromIntegral
    . concatMap toNumbers
    $ NonEmpty.toList parts
  where
    toNumbers :: Part.Part -> [Natural.Natural]
    toNumbers (Part.Numeric n) = [n]
    toNumbers Part.Wildcard = []
