module CabalGild.Type.Language where

import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Language.Haskell.Extension as Extension

-- | This type exists to provide an 'Ord' instance for
-- 'Language.Language', which was added in @Cabal-syntax-3.10.1.0@.
newtype Language = Language
  { unwrap :: Extension.Language
  }
  deriving (Eq, Show)

instance Ord Language where
  compare x y = case (unwrap x, unwrap y) of
    (Extension.Haskell98, Extension.Haskell98) -> EQ
    (Extension.Haskell98, _) -> LT
    (Extension.Haskell2010, Extension.Haskell2010) -> EQ
    (Extension.Haskell2010, Extension.Haskell98) -> GT
    (Extension.Haskell2010, _) -> LT
    (Extension.GHC2021, Extension.GHC2021) -> EQ
    (Extension.GHC2021, Extension.UnknownLanguage _) -> LT
    (Extension.GHC2021, _) -> GT
    (Extension.UnknownLanguage s, Extension.UnknownLanguage t) -> compare s t
    (Extension.UnknownLanguage _, _) -> GT

instance Parsec.Parsec Language where
  parsec = Language <$> Parsec.parsec

instance Pretty.Pretty Language where
  pretty = Pretty.pretty . unwrap
