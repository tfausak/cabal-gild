{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CabalGild.Type.List where

import qualified CabalGild.Extra.List as List
import qualified Data.Proxy as Proxy
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.Compat.Newtype as Newtype
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Text.PrettyPrint as PrettyPrint

-- | A thin wrapper around 'Newtypes.List'. This is used to define custom
-- instances of 'Pretty.Pretty' that use trailing commas when the Cabal spec
-- version is recent enough (at least @2.2@).
newtype List s b a = List
  { unwrap :: Newtypes.List s b a
  }

instance Newtype.Newtype [a] (List s b a) where
  pack = List . Newtype.pack
  unpack = Newtype.unpack . unwrap

instance
  ( Newtype.Newtype a b,
    Parsec.Parsec b,
    Newtypes.Sep s
  ) =>
  Parsec.Parsec (List s b a)
  where
  parsec = List <$> Parsec.parsec

instance
  ( Newtype.Newtype a b,
    Pretty.Pretty b,
    Newtypes.Sep s
  ) =>
  Pretty.Pretty (List s b a)
  where
  pretty = Pretty.prettyVersioned CabalSpecVersion.CabalSpecV1_0
  prettyVersioned v =
    Newtypes.prettySep (Proxy.Proxy @s)
      . fmap (Pretty.prettyVersioned @b v . Newtype.pack)
      . Newtype.unpack

-- | Overlaps the more general instance in order to use trailing commas when
-- possible.
instance
  {-# OVERLAPPING #-}
  ( Newtype.Newtype a b,
    Pretty.Pretty b
  ) =>
  Pretty.Pretty (List Newtypes.CommaFSep b a)
  where
  pretty = Pretty.prettyVersioned CabalSpecVersion.CabalSpecV1_0
  prettyVersioned v =
    ( \xs ->
        if List.compareLength xs 1 == GT && v >= CabalSpecVersion.CabalSpecV2_2
          then PrettyPrint.fsep $ fmap (<> PrettyPrint.comma) xs
          else Newtypes.prettySep (Proxy.Proxy @Newtypes.CommaFSep) xs
    )
      . fmap (Pretty.prettyVersioned @b v . Newtype.pack)
      . Newtype.unpack
