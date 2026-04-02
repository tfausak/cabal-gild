{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Type.Mode where

import Bluefin.Eff (Eff, (:>))
import qualified Bluefin.Exception as Exception
import qualified CabalGild.Unstable.Exception.InvalidMode as InvalidMode
import qualified Control.Exception as E

-- | Represents the mode of the command line utility.
data Mode
  = -- | Just determine if the input is already formatted.
    Check
  | -- | Format the input.
    Format
  deriving (Eq, Show)

-- | Attempts to parse a string as a mode.
fromString :: (eX :> es) => Exception.Exception E.SomeException eX -> String -> Eff es Mode
fromString ex s = case s of
  "check" -> pure Check
  "format" -> pure Format
  _ -> Exception.throw ex . E.toException $ InvalidMode.fromString s
