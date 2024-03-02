module CabalGild.Unstable.Type.ModuleReexport where

import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.ModuleReexport as ModuleReexport

-- | This type exists to provide an 'Ord' instance for
-- 'ModuleReexport.ModuleReexport', which was added in @Cabal-syntax-3.10.1.0@.
newtype ModuleReexport = ModuleReexport
  { unwrap :: ModuleReexport.ModuleReexport
  }
  deriving (Eq, Show)

instance Ord ModuleReexport where
  compare =
    Ord.comparing $
      (\(ModuleReexport.ModuleReexport mpn mn1 mn2) -> (mpn, mn1, mn2))
        . unwrap

instance Parsec.Parsec ModuleReexport where
  parsec = ModuleReexport <$> Parsec.parsec

instance Pretty.Pretty ModuleReexport where
  pretty = Pretty.pretty . unwrap
