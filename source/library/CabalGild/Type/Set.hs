{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CabalGild.Type.Set where

import qualified CabalGild.Extra.List as List
import qualified Data.Proxy as Proxy
import qualified Data.Set as Set
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.Compat.Newtype as Newtype
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Text.PrettyPrint as PrettyPrint

-- | A thin wrapper around 'Newtypes.Set''. This is used to define custom
-- instances of 'Pretty.Pretty' that use trailing commas when the Cabal spec
-- version is recent enough (at least @2.2@).
newtype Set s b a = Set
  { unwrap :: Newtypes.Set' s b a
  }

instance Newtype.Newtype (Set.Set a) (Set s b a) where
  pack = Set . Newtype.pack
  unpack = Newtype.unpack . unwrap

instance
  ( Newtype.Newtype a b,
    Ord a,
    Parsec.Parsec b,
    Newtypes.Sep s
  ) =>
  Parsec.Parsec (Set s b a)
  where
  parsec = Set <$> Parsec.parsec

instance
  ( Newtype.Newtype a b,
    Pretty.Pretty b,
    Newtypes.Sep s
  ) =>
  Pretty.Pretty (Set s b a)
  where
  pretty = Pretty.prettyVersioned CabalSpecVersion.CabalSpecV1_0
  prettyVersioned v =
    Newtypes.prettySep (Proxy.Proxy @s)
      . fmap (Pretty.prettyVersioned @b v . Newtype.pack)
      . Set.toAscList
      . Newtype.unpack

-- | Overlaps the more general instance in order to use trailing commas when
-- possible.
instance
  {-# OVERLAPPING #-}
  ( Newtype.Newtype a b,
    Pretty.Pretty b
  ) =>
  Pretty.Pretty (Set Newtypes.CommaFSep b a)
  where
  pretty = Pretty.prettyVersioned CabalSpecVersion.CabalSpecV1_0
  prettyVersioned v =
    ( \xs ->
        if List.compareLength xs 1 == GT && v >= CabalSpecVersion.CabalSpecV2_2
          then PrettyPrint.fsep $ fmap (<> PrettyPrint.comma) xs
          else Newtypes.prettySep (Proxy.Proxy @Newtypes.CommaFSep) xs
    )
      . fmap (Pretty.prettyVersioned @b v . Newtype.pack)
      . Set.toAscList
      . Newtype.unpack

-- | Overlaps the more general instance in order to use trailing commas when
-- possible.
instance
  {-# OVERLAPPING #-}
  ( Newtype.Newtype a b,
    Pretty.Pretty b
  ) =>
  Pretty.Pretty (Set Newtypes.CommaVCat b a)
  where
  pretty = Pretty.prettyVersioned CabalSpecVersion.CabalSpecV1_0
  prettyVersioned v =
    ( \xs ->
        if List.compareLength xs 1 == GT && v >= CabalSpecVersion.CabalSpecV2_2
          then PrettyPrint.vcat $ fmap (<> PrettyPrint.comma) xs
          else Newtypes.prettySep (Proxy.Proxy @Newtypes.CommaVCat) xs
    )
      . fmap (Pretty.prettyVersioned @b v . Newtype.pack)
      . Set.toAscList
      . Newtype.unpack
