{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Refactoring.ExpandExposedModules where

import CabalGild.Monad
import CabalGild.Pragma
import CabalGild.Refactoring.Type
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Fields as C
import qualified Distribution.ModuleName as C
import Distribution.Utils.Generic (toUTF8BS)
import qualified System.FilePath as FilePath

refactoringExpandExposedModules :: FieldRefactoring
refactoringExpandExposedModules C.Section {} = pure Nothing
refactoringExpandExposedModules (C.Field name@(C.Name (_, _, pragmas) _n) fls) = do
  dirs <- parse pragmas
  files <- (traverse . Lens._1) getFiles dirs

  let newModules :: [C.FieldLine CommentsPragmas]
      newModules =
        Maybe.catMaybes
          [ return $ C.FieldLine emptyCommentsPragmas $ toUTF8BS $ List.intercalate "." parts
            | (files', mns) <- files,
              file <- files',
              let parts = FilePath.splitDirectories $ FilePath.dropExtension file,
              all C.validModuleComponent parts,
              let mn = C.fromComponents parts, -- TODO: don't use fromComponents
              mn `notElem` mns
          ]

  pure $ case newModules of
    [] -> Nothing
    _ -> Just (C.Field name (newModules ++ fls))
  where
    parse :: (MonadCabalGild r m) => [FieldPragma] -> m [(FilePath, [C.ModuleName])]
    parse = fmap mconcat . traverse go
      where
        go :: (MonadCabalGild r m) => FieldPragma -> m [(FilePath, [C.ModuleName])]
        go (PragmaExpandModules fp mns) = return [(fp, mns)]
        go p = do
          displayWarning $ "Skipped pragma " ++ show p
          return []
