module CabalGild.Unstable.Class.Warning where

-- | A warning, which is similar to an 'Control.Exception.Exception'.
class (Show w) => Warning w where
  displayWarning :: w -> String
  displayWarning = show
