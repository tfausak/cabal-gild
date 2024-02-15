{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CabalGild.Compat.Parsec where

#if !MIN_VERSION_parsec(3, 1, 17)

import qualified Control.Monad.Catch as Exception
import qualified Text.Parsec.Error as Parsec

instance Exception.Exception Parsec.ParseError

#endif
