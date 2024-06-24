module CabalGild.Unstable.Class.Warning where

import qualified Data.Typeable as Typeable

-- | A warning, which can be emitted without stopping the program (like an
-- 'Control.Exception.Exception').
class (Eq w, Show w, Typeable.Typeable w) => Warning w where
  -- | Displays the warning in a human-readable format. Defaults to 'show'.
  displayWarning :: w -> String
  displayWarning = show
