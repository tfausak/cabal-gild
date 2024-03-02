{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CabalGild.Unstable.Type.SomeParsecParser where

import qualified CabalGild.Unstable.Type.List as List
import qualified CabalGild.Unstable.Type.Set as Set
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Text.PrettyPrint as PrettyPrint

-- | This type bundles up a parser and a pretty printer for some type.
-- Typically they will just be monomorphic versions of 'Parsec.parsec' and
-- 'Pretty.prettyVersioned'.
data SomeParsecParser
  = forall a.
  SomeParsecParser
  { parsec :: Parsec.ParsecParser a,
    pretty :: CabalSpecVersion.CabalSpecVersion -> a -> PrettyPrint.Doc
  }

-- | Creates a new parser for the given 'List.List' of values.
list ::
  forall s b a.
  (Parsec.Parsec (List.List s b a), Pretty.Pretty (List.List s b a)) =>
  SomeParsecParser
list =
  SomeParsecParser
    { parsec = Parsec.parsec @(List.List s b a),
      pretty = Pretty.prettyVersioned
    }

-- | Creates a new parser for the given 'Set.Set' of values.
set ::
  forall s b a.
  (Parsec.Parsec (Set.Set s b a), Pretty.Pretty (Set.Set s b a)) =>
  SomeParsecParser
set =
  SomeParsecParser
    { parsec = Parsec.parsec @(Set.Set s b a),
      pretty = Pretty.prettyVersioned
    }
