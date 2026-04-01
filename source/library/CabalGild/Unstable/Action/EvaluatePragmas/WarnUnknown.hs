module CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown where

import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Control.Monad as Monad
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
warnComment c = case Parsec.simpleParsecBS $ Comment.value c of
  Just (Pragma.Pragma (PragmaName name)) ->
    Monad.unless (name `elem` knownPragmaNames) $
      MonadWarn.warnLn $ "warning: unknown pragma \"" <> name <> "\""
  Nothing -> pure ()

-- | A type that parses just the pragma name (the first word after "cabal-gild:").
newtype PragmaName = PragmaName String

instance Parsec.Parsec PragmaName where
  parsec = do
    cs <- CharParsing.some (CharParsing.satisfy $ \c -> c /= ' ' && c /= '\t' && c /= '\n')
    -- Consume any remaining characters without failing
    Monad.void $ CharParsing.many CharParsing.anyChar
    pure $ PragmaName cs

-- | The set of known pragma names.
knownPragmaNames :: [String]
knownPragmaNames = ["discover", "require", "version"]
