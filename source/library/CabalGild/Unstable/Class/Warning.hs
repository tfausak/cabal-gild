module CabalGild.Unstable.Class.Warning where

-- | A warning, which can be emitted without stopping the program (like an
-- 'Control.Exception.Exception').
class (Show w) => Warning w where
  -- | Displays the warning in a human-readable format. Defaults to 'show'.
  displayWarning :: w -> String
  displayWarning = show
