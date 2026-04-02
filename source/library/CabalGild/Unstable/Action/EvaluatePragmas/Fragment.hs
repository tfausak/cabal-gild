{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Action.EvaluatePragmas.Fragment where

import Bluefin.Eff (Eff, (:>))
import qualified CabalGild.Unstable.Effect.Read as Read
import qualified CabalGild.Unstable.Effect.Warn as Warn
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.SectionArg as SectionArg
import qualified CabalGild.Unstable.Extra.String as String
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Input as Input
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Control.Monad as Monad
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Utils.Generic as Utils
import Prelude hiding (Read, read)
import qualified System.FilePath as FilePath

-- | High level wrapper that traverses all fields and evaluates fragment pragmas.
run ::
  (eR :> es, eW :> es) =>
  Read.Read eR ->
  Warn.Warn eW ->
  FilePath ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  Eff es ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run readH warnH p (fs, cs) = (,) <$> traverse (field readH warnH p) fs <*> pure cs

-- | Evaluates fragment pragmas on a single field or section. For sections,
-- recurses into child fields.
field ::
  (eR :> es, eW :> es) =>
  Read.Read eR ->
  Warn.Warn eW ->
  FilePath ->
  Fields.Field (p, Comments.Comments q) ->
  Eff es (Fields.Field (p, Comments.Comments q))
field readH warnH p f = case f of
  Fields.Field n _ -> do
    result <- tryFragment readH warnH p n
    case result of
      Nothing -> pure f
      Just fragmentFields -> case fragmentFields of
        [] -> do
          Warn.warnLn warnH "warning: fragment file is empty"
          pure f
        Fields.Section {} : _ -> do
          Warn.warnLn warnH
            "warning: fragment contains a section, but the pragma is on a field"
          pure f
        Fields.Field (Fields.Name _ n') fls' : _ -> do
          if Name.value n == n'
            then
              let position = fst $ Name.annotation n
               in pure . Fields.Field n $
                    fmap ((position, Comments.empty) <$) fls'
            else do
              Warn.warnLn warnH $
                "warning: fragment contains field \""
                  <> String.fromUtf8 n'
                  <> "\", but expected \""
                  <> String.fromUtf8 (Name.value n)
                  <> "\""
              pure f
  Fields.Section n sas children -> do
    result <- tryFragment readH warnH p n
    case result of
      Nothing -> Fields.Section n sas <$> traverse (field readH warnH p) children
      Just fragmentFields -> case fragmentFields of
        [] -> do
          Warn.warnLn warnH "warning: fragment file is empty"
          Fields.Section n sas <$> traverse (field readH warnH p) children
        Fields.Field (Fields.Name _ n') _ : _ -> do
          Warn.warnLn warnH $
            "warning: fragment contains a field \""
              <> String.fromUtf8 n'
              <> "\", but the pragma is on a section"
          Fields.Section n sas <$> traverse (field readH warnH p) children
        Fields.Section (Fields.Name _ n') sas' fs' : _ -> do
          if Name.value n == n' && fmap SectionArg.value sas == fmap SectionArg.value sas'
            then
              let position = fst $ Name.annotation n
               in pure . Fields.Section n sas $
                    fmap (fmap (const (position, Comments.empty))) fs'
            else do
              Warn.warnLn warnH $
                "warning: fragment contains section \""
                  <> showSection n' sas'
                  <> "\", but expected \""
                  <> showSection (Name.value n) sas
                  <> "\""
              Fields.Section n sas <$> traverse (field readH warnH p) children

-- | Tries to find and read a fragment pragma from the last "before" comment on
-- a field/section name. Returns 'Nothing' if no fragment pragma is present.
-- Returns 'Just fields' with the parsed fragment content on success, or
-- 'Nothing' on read/parse failure (with a warning emitted).
tryFragment ::
  (eR :> es, eW :> es) =>
  Read.Read eR ->
  Warn.Warn eW ->
  FilePath ->
  Fields.Name (p, Comments.Comments q) ->
  Eff es (Maybe [Fields.Field Parsec.Position])
tryFragment readH warnH p n = do
  let comments = Comments.toList . snd $ Name.annotation n
  case Utils.safeLast comments of
    Nothing -> pure Nothing
    Just comment -> case Parsec.simpleParsecBS $ Comment.value comment of
      Nothing -> pure Nothing
      Just (Pragma.Pragma (Fragment token)) -> do
        let root = FilePath.takeDirectory p
            path = FilePath.normalise $ FilePath.combine root token
        result <- Read.tryRead readH (Input.File path)
        case result of
          Left _ -> do
            Warn.warnLn warnH $
              "warning: could not read fragment " <> show path
            pure Nothing
          Right contents -> case Fields.readFields contents of
            Left _ -> do
              Warn.warnLn warnH $
                "warning: could not parse fragment " <> show path
              pure Nothing
            Right fields -> pure $ Just fields

-- | Renders a section name and its arguments as a human-readable string for
-- use in warning messages.
showSection :: Fields.FieldName -> [Fields.SectionArg a] -> String
showSection n [] = String.fromUtf8 n
showSection n args =
  String.fromUtf8 n
    <> " "
    <> unwords (fmap (String.fromUtf8 . SectionArg.value) args)

-- | The fragment pragma type. Parsed from @-- cabal-gild: fragment FILE@.
newtype Fragment = Fragment String
  deriving (Eq, Show)

instance Parsec.Parsec Fragment where
  parsec = do
    Monad.void $ CharParsing.string "fragment"
    CharParsing.skipSpaces1
    Fragment . Newtypes.getToken' <$> Parsec.parsec
