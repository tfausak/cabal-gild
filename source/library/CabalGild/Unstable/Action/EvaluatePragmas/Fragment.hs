{-# LANGUAGE ScopedTypeVariables #-}

module CabalGild.Unstable.Action.EvaluatePragmas.Fragment where

import qualified CabalGild.Unstable.Class.MonadRead as MonadRead
import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.SectionArg as SectionArg
import qualified CabalGild.Unstable.Extra.String as String
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Input as Input
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Utils.Generic as Utils
import qualified System.FilePath as FilePath

-- | High level wrapper that traverses all fields and evaluates fragment pragmas.
run ::
  (Exception.MonadCatch m, MonadRead.MonadRead m, MonadWarn.MonadWarn m) =>
  FilePath ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  m ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run p (fs, cs) = (,) <$> traverse (field p) fs <*> pure cs

-- | Evaluates fragment pragmas on a single field or section. For sections,
-- recurses into child fields.
field ::
  (Exception.MonadCatch m, MonadRead.MonadRead m, MonadWarn.MonadWarn m) =>
  FilePath ->
  Fields.Field (p, Comments.Comments q) ->
  m (Fields.Field (p, Comments.Comments q))
field p f = case f of
  Fields.Field n _ -> do
    result <- tryFragment p n
    case result of
      Nothing -> pure f
      Just fragmentFields -> case fragmentFields of
        [] -> do
          MonadWarn.warnLn "warning: fragment file is empty"
          pure f
        Fields.Section {} : _ -> do
          MonadWarn.warnLn $
            "warning: fragment contains a section, but the pragma is on a field"
          pure f
        Fields.Field (Fields.Name _ n') fls' : _ -> do
          if Name.value n == n'
            then
              let position = fst $ Name.annotation n
               in pure . Fields.Field n $
                    fmap ((position, Comments.empty) <$) fls'
            else do
              MonadWarn.warnLn $
                "warning: fragment contains field \""
                  <> String.fromUtf8 n'
                  <> "\", but expected \""
                  <> String.fromUtf8 (Name.value n)
                  <> "\""
              pure f
  Fields.Section n sas _ -> do
    result <- tryFragment p n
    case result of
      Nothing -> Fields.Section n sas <$> traverse (field p) (sectionFields f)
      Just fragmentFields -> case fragmentFields of
        [] -> do
          MonadWarn.warnLn "warning: fragment file is empty"
          pure f
        Fields.Field (Fields.Name _ n') _ : _ -> do
          MonadWarn.warnLn $
            "warning: fragment contains a field \""
              <> String.fromUtf8 n'
              <> "\", but the pragma is on a section"
          pure f
        Fields.Section (Fields.Name _ n') sas' fs' : _ -> do
          if Name.value n == n' && fmap SectionArg.value sas == fmap SectionArg.value sas'
            then
              let position = fst $ Name.annotation n
               in pure . Fields.Section n sas $
                    fmap (fmap (const (position, Comments.empty))) fs'
            else do
              MonadWarn.warnLn $
                "warning: fragment section name or args do not match"
              pure f

-- | Extracts child fields from a section. Needed because the Section pattern
-- match in the outer case already destructured, but we need the original for
-- the fallback.
sectionFields :: Fields.Field a -> [Fields.Field a]
sectionFields (Fields.Section _ _ fs) = fs
sectionFields _ = []

-- | Tries to find and read a fragment pragma from the last "before" comment on
-- a field/section name. Returns 'Nothing' if no fragment pragma is present.
-- Returns 'Just fields' with the parsed fragment content on success, or
-- 'Nothing' on read/parse failure (with a warning emitted).
tryFragment ::
  (Exception.MonadCatch m, MonadRead.MonadRead m, MonadWarn.MonadWarn m) =>
  FilePath ->
  Fields.Name (p, Comments.Comments q) ->
  m (Maybe [Fields.Field Parsec.Position])
tryFragment p n = do
  let comments = Comments.before . snd $ Name.annotation n
  case Utils.safeLast comments of
    Nothing -> pure Nothing
    Just comment -> case Parsec.simpleParsecBS $ Comment.value comment of
      Nothing -> pure Nothing
      Just (Pragma.Pragma (Fragment token)) -> do
        let root = FilePath.takeDirectory p
            path = FilePath.normalise $ FilePath.combine root token
        result <- Exception.try $ MonadRead.read (Input.File path)
        case result of
          Left (_ :: Exception.SomeException) -> do
            MonadWarn.warnLn $
              "warning: could not read fragment " <> show path
            pure Nothing
          Right contents -> case Fields.readFields contents of
            Left _ -> do
              MonadWarn.warnLn $
                "warning: could not parse fragment " <> show path
              pure Nothing
            Right fields -> pure $ Just fields

-- | The fragment pragma type. Parsed from @-- cabal-gild: fragment FILE@.
newtype Fragment = Fragment String
  deriving (Eq, Show)

instance Parsec.Parsec Fragment where
  parsec = do
    Monad.void $ CharParsing.string "fragment"
    CharParsing.skipSpaces1
    Fragment . Newtypes.getToken' <$> Parsec.parsec
