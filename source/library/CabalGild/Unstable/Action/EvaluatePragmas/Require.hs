module CabalGild.Unstable.Action.EvaluatePragmas.Require where

import qualified CabalGild.Unstable.Exception.UnsatisfiedRequire as UnsatisfiedRequire
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified CabalGild.Unstable.Type.VersionRange as VR
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Version as Version
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Types.Version as CabalVersion
import qualified Distribution.Types.VersionRange as CabalVR
import qualified Numeric.Natural as Natural
import qualified Paths_cabal_gild as This

run ::
  (Exception.MonadThrow m) =>
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  m ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run (fs, cs) = (,) <$> traverse field fs <*> checkRequire cs

field ::
  (Exception.MonadThrow m) =>
  Fields.Field (p, Comments.Comments q) ->
  m (Fields.Field (p, Comments.Comments q))
field f = case f of
  Fields.Field n fls -> do
    requireComments . snd $ Name.annotation n
    mapM_ (requireComments . snd . FieldLine.annotation) fls
    pure f
  Fields.Section n sas fs -> do
    requireComments . snd $ Name.annotation n
    Fields.Section n sas <$> traverse field fs

requireComments ::
  (Exception.MonadThrow m) =>
  Comments.Comments q ->
  m ()
requireComments cs = do
  Monad.void $ checkRequire (Comments.before cs)
  Monad.void $ checkRequire (Comments.after cs)

-- | Walks a list of comments. When a require pragma is found, validates that
-- the current cabal-gild version satisfies the specified range.
checkRequire ::
  (Exception.MonadThrow m) =>
  [Comment.Comment q] ->
  m [Comment.Comment q]
checkRequire [] = pure []
checkRequire (c : cs) = case Parsec.simpleParsecBS $ Comment.value c of
  Just (Pragma.Pragma (Require vr)) -> do
    let cabalVR = toCabalVersionRange vr
        currentVersion = CabalVersion.mkVersion $ Version.versionBranch This.version
    Monad.unless (CabalVR.withinRange currentVersion cabalVR) $
      Exception.throwM
        UnsatisfiedRequire.UnsatisfiedRequire
          { UnsatisfiedRequire.actual = This.version,
            UnsatisfiedRequire.range = vr
          }
    (c :) <$> checkRequire cs
  Nothing -> (c :) <$> checkRequire cs

newtype Require = Require VR.VersionRange
  deriving (Eq, Show)

instance Parsec.Parsec Require where
  parsec = do
    Monad.void $ CharParsing.string "require"
    CharParsing.skipSpaces1
    Require <$> Parsec.parsec

-- | Converts the project's 'VR.VersionRange' to Cabal-syntax's
-- 'CabalVR.VersionRange' for evaluation with 'CabalVR.withinRange'.
toCabalVersionRange :: VR.VersionRange -> CabalVR.VersionRange
toCabalVersionRange = toCabalComplex

toCabalComplex :: VR.Complex VR.Simple -> CabalVR.VersionRange
toCabalComplex x = case x of
  VR.Par c -> toCabalComplex c
  VR.And l r -> CabalVR.intersectVersionRanges (toCabalSimple l) (toCabalComplex r)
  VR.Or l r -> CabalVR.unionVersionRanges (toCabalSimple l) (toCabalComplex r)
  VR.Simple s -> toCabalSimple s

toCabalSimple :: VR.Simple -> CabalVR.VersionRange
toCabalSimple x = case x of
  VR.Any -> CabalVR.anyVersion
  VR.None -> CabalVR.noVersion
  VR.Op op vs -> toCabalOp op vs

toCabalOp :: VR.Operator -> VR.Versions -> CabalVR.VersionRange
toCabalOp op vs = case vs of
  VR.One v -> toCabalOpOne op v
  VR.Set s -> case Set.toList s of
    [] -> CabalVR.noVersion
    v : rest -> foldr (CabalVR.unionVersionRanges . toCabalOpOne op) (toCabalOpOne op v) rest

toCabalOpOne :: VR.Operator -> VR.Version -> CabalVR.VersionRange
toCabalOpOne op v = case op of
  VR.Caret -> CabalVR.majorBoundVersion cv
  VR.Ge -> CabalVR.orLaterVersion cv
  VR.Gt -> CabalVR.laterVersion cv
  VR.Le -> CabalVR.orEarlierVersion cv
  VR.Lt -> CabalVR.earlierVersion cv
  VR.Eq
    | hasWildcard v -> CabalVR.withinVersion cv
    | otherwise -> CabalVR.thisVersion cv
  where
    cv = toCabalVersion v

hasWildcard :: VR.Version -> Bool
hasWildcard (VR.MkVersion parts) = any isWildcard $ NonEmpty.toList parts
  where
    isWildcard VR.Wildcard = True
    isWildcard _ = False

toCabalVersion :: VR.Version -> CabalVersion.Version
toCabalVersion (VR.MkVersion parts) =
  CabalVersion.mkVersion
    . map fromIntegral
    . concatMap toNumbers
    $ NonEmpty.toList parts
  where
    toNumbers :: VR.Part -> [Natural.Natural]
    toNumbers (VR.Numeric n) = [n]
    toNumbers VR.Wildcard = []
