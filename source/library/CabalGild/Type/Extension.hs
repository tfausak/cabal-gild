module CabalGild.Type.Extension where

import qualified Data.Char as Char
import qualified Data.Ord as Ord
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Language.Haskell.Extension as Extension

-- | This type exists to provide a different 'Ord' instance for
-- 'Extension.Extension'. The instance provided by @Cabal-syntax@ sorts things
-- in a surprising order. This one sorts things alphabetically with "enable"
-- extensions before "disable" ones.
newtype Extension = Extension
  { unwrap :: Extension.Extension
  }
  deriving (Eq, Show)

instance Ord Extension where
  compare = Ord.comparing $ \e -> (isDisable $ unwrap e, Pretty.prettyShow e)

instance Parsec.Parsec Extension where
  parsec = Extension <$> Parsec.parsec

instance Pretty.Pretty Extension where
  pretty = Pretty.pretty . unwrap

-- | Returns 'True' if the given extension is either a
-- 'Extension.DisableExtension' or it's an 'Extension.UnknownExtension' that
-- starts with @"No"@ followed by an uppercase letter.
isDisable :: Extension.Extension -> Bool
isDisable e = case e of
  Extension.EnableExtension _ -> False
  Extension.DisableExtension _ -> True
  Extension.UnknownExtension s -> case s of
    'N' : 'o' : c : _ -> Char.isUpper c
    _ -> False
