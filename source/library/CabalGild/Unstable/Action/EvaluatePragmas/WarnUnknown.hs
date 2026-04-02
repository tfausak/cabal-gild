{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown where

import qualified CabalGild.Unstable.Action.EvaluatePragmas.Discover as Discover
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Fragment as Fragment
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Require as Require
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Version as Version
import qualified CabalGild.Unstable.Effect.Warn as Warn
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import Bluefin.Eff (Eff, (:>))
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec

run ::
  (eW :> es) =>
  Warn.Warn eW ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  Eff es ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run warnH (fs, cs) = do
  mapM_ (field warnH) fs
  mapM_ (warnComment warnH) cs
  pure (fs, cs)

field ::
  (eW :> es) =>
  Warn.Warn eW ->
  Fields.Field (p, Comments.Comments q) ->
  Eff es ()
field warnH f = case f of
  Fields.Field n fls -> do
    warnComments warnH . snd $ Name.annotation n
    mapM_ (warnComments warnH . snd . FieldLine.annotation) fls
  Fields.Section n _ fs -> do
    warnComments warnH . snd $ Name.annotation n
    mapM_ (field warnH) fs

warnComments ::
  (eW :> es) =>
  Warn.Warn eW ->
  Comments.Comments q ->
  Eff es ()
warnComments warnH cs = do
  mapM_ (warnComment warnH) (Comments.before cs)
  mapM_ (warnComment warnH) (Comments.after cs)

warnComment ::
  (eW :> es) =>
  Warn.Warn eW ->
  Comment.Comment q ->
  Eff es ()
warnComment warnH c =
  let bs = Comment.value c
   in case Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma PragmaBody) of
        Nothing -> pure ()
        Just (Pragma.Pragma (PragmaBody body))
          | isKnownPragma bs -> pure ()
          | otherwise ->
              Warn.warnLn warnH $ "warning: unknown pragma \"" <> body <> "\""

-- | Checks whether a comment parses as any known pragma type.
isKnownPragma :: ByteString.ByteString -> Bool
isKnownPragma bs =
  Maybe.isJust (Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma Discover.Discover))
    || Maybe.isJust (Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma Fragment.Fragment))
    || Maybe.isJust (Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma Require.Require))
    || Maybe.isJust (Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma Version.Version))

-- | Captures all text after the "cabal-gild:" prefix.
newtype PragmaBody = PragmaBody String

instance Parsec.Parsec PragmaBody where
  parsec = PragmaBody <$> CharParsing.many CharParsing.anyChar
