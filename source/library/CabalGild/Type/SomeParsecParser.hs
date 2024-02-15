{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CabalGild.Type.SomeParsecParser where

import qualified CabalGild.Type.List as List
import qualified CabalGild.Type.Set as Set
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty

data SomeParsecParser
  = forall c s b a.
    (Parsec.Parsec (c s b a), Pretty.Pretty (c s b a)) =>
    SomeParsecParser (Parsec.ParsecParser (c s b a))

list ::
  forall s b a.
  (Parsec.Parsec (List.List s b a), Pretty.Pretty (List.List s b a)) =>
  SomeParsecParser
list = SomeParsecParser $ Parsec.parsec @(List.List s b a)

set ::
  forall s b a.
  (Parsec.Parsec (Set.Set s b a), Pretty.Pretty (Set.Set s b a)) =>
  SomeParsecParser
set = SomeParsecParser $ Parsec.parsec @(Set.Set s b a)
