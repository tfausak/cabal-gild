{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Type.Leniency where

import Bluefin.Eff (Eff, (:>))
import qualified Bluefin.Exception as Exception
import qualified CabalGild.Unstable.Exception.InvalidLeniency as InvalidLeniency
import qualified Control.Exception as E

-- | Represents the leniency of a setting. In other words, should something be
-- lenient\/permissive or strict\/pedantic?
data Leniency
  = Lenient
  | Strict
  deriving (Eq, Show)

-- | Attempts to parse a string as a 'Leniency'.
fromString :: (eX :> es) => Exception.Exception E.SomeException eX -> String -> Eff es Leniency
fromString ex s = case s of
  "lenient" -> pure Lenient
  "strict" -> pure Strict
  _ -> Exception.throw ex . E.toException $ InvalidLeniency.fromString s
