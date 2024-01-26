{-# LANGUAGE OverloadedStrings #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalFmt.Fields.Extensions
  ( otherExtensionsF,
    defaultExtensionsF,
  )
where

import CabalFmt.Fields
import CabalFmt.Prelude
import qualified Distribution.FieldGrammar as C
import qualified Distribution.Parsec as C
import qualified Distribution.Pretty as C
import qualified Language.Haskell.Extension as C
import qualified Text.PrettyPrint as PP

otherExtensionsF :: FieldDescrs () ()
otherExtensionsF = singletonF "other-extensions" pretty parse

defaultExtensionsF :: FieldDescrs () ()
defaultExtensionsF = singletonF "default-extensions" pretty parse

parse :: (C.CabalParsing m) => m [C.Extension]
parse = unpack' (C.alaList' C.FSep C.MQuoted) <$> C.parsec

pretty :: [C.Extension] -> PP.Doc
pretty = PP.vcat . map C.pretty . sortOn C.prettyShow
