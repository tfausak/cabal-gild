module CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown where

import qualified CabalGild.Unstable.Action.EvaluatePragmas.Discover as Discover
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Require as Require
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Version as Version
import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec

run ::
  (MonadWarn.MonadWarn m) =>
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  m ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run (fs, cs) = do
  mapM_ field fs
  mapM_ warnComment cs
  pure (fs, cs)

field ::
  (MonadWarn.MonadWarn m) =>
  Fields.Field (p, Comments.Comments q) ->
  m ()
field f = case f of
  Fields.Field n fls -> do
    warnComments . snd $ Name.annotation n
    mapM_ (warnComments . snd . FieldLine.annotation) fls
  Fields.Section n _ fs -> do
    warnComments . snd $ Name.annotation n
    mapM_ field fs

warnComments ::
  (MonadWarn.MonadWarn m) =>
  Comments.Comments q ->
  m ()
warnComments cs = do
  mapM_ warnComment (Comments.before cs)
  mapM_ warnComment (Comments.after cs)

warnComment ::
  (MonadWarn.MonadWarn m) =>
  Comment.Comment q ->
  m ()
warnComment c =
  let bs = Comment.value c
   in case Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma PragmaBody) of
        Nothing -> pure ()
        Just (Pragma.Pragma (PragmaBody body))
          | isKnownPragma bs -> pure ()
          | otherwise ->
              MonadWarn.warnLn $ "warning: unknown pragma \"" <> body <> "\""

-- | Checks whether a comment parses as any known pragma type.
isKnownPragma :: ByteString.ByteString -> Bool
isKnownPragma bs =
  Maybe.isJust (Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma Discover.Discover))
    || Maybe.isJust (Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma Require.Require))
    || Maybe.isJust (Parsec.simpleParsecBS bs :: Maybe (Pragma.Pragma Version.Version))

-- | Captures all text after the "cabal-gild:" prefix.
newtype PragmaBody = PragmaBody String

instance Parsec.Parsec PragmaBody where
  parsec = PragmaBody <$> CharParsing.many CharParsing.anyChar
