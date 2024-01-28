{-# LANGUAGE OverloadedStrings #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Fields.SourceFiles
  ( sourceFilesF,
    fileFields,
  )
where

-- Make sure we explicitly use Posix's splitDirectories
-- when parsing glob syntax since only `/` is valid, and not '\\'

import CabalGild.Fields
import CabalGild.Prelude
import qualified Distribution.FieldGrammar as C
import qualified Distribution.Fields as C
import qualified Distribution.Parsec as C
import qualified Distribution.Pretty as C
import qualified System.FilePath.Posix as Posix (splitDirectories)
import qualified Text.PrettyPrint as PP

sourceFilesF :: [FieldDescrs () ()]
sourceFilesF =
  [ singletonF f pretty parse
    | f <- fileFields
  ]

fileFields :: [C.FieldName]
fileFields =
  [ "extra-source-files",
    "extra-doc-files",
    "data-files",
    "license-files",
    "asm-sources",
    "cmm-sources",
    "c-sources",
    "cxx-sources",
    "js-sources",
    "includes",
    "install-includes"
  ]

parse :: (C.CabalParsing m) => m [FilePath]
parse = unpack' (C.alaList' C.VCat C.FilePathNT) <$> C.parsec

pretty :: [FilePath] -> PP.Doc
pretty =
  PP.vcat
    . map C.showFilePath
    . nub
    . sortBy (cmp `on` map strToLower . Posix.splitDirectories)
  where
    cmp :: (Ord a) => [a] -> [a] -> Ordering
    cmp a b = case dropCommonPrefix a b of
      ([], []) -> EQ
      ([], _ : _) -> LT
      (_ : _, []) -> GT
      (a', b') -> compare a' b'

strToLower :: String -> String
strToLower = map toLower

dropCommonPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
dropCommonPrefix [] [] = ([], [])
dropCommonPrefix [] ys = ([], ys)
dropCommonPrefix xs [] = (xs, [])
dropCommonPrefix xs@(x : xs') ys@(y : ys')
  | x == y = dropCommonPrefix xs' ys'
  | otherwise = (xs, ys)
