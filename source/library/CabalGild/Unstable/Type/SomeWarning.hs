{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module CabalGild.Unstable.Type.SomeWarning where

import qualified CabalGild.Unstable.Class.Warning as Warning

-- | Some particular 'Warning.Warning', wrapped up like a
-- 'Control.Exception.SomeException'.
data SomeWarning
  = forall w. (Warning.Warning w) => SomeWarning w

deriving instance Show SomeWarning

instance Warning.Warning SomeWarning where
  displayWarning (SomeWarning w) = Warning.displayWarning w
