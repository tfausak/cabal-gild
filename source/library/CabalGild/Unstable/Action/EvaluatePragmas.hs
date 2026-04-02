{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Action.EvaluatePragmas where

import Bluefin.Eff (Eff, (:>))
import qualified Bluefin.Exception as Exception
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Discover as EvaluateDiscover
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Fragment as EvaluateFragment
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Require as EvaluateRequire
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Version as EvaluateVersion
import qualified CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown as WarnUnknown
import qualified CabalGild.Unstable.Effect.Read as Read
import qualified CabalGild.Unstable.Effect.Walk as Walk
import qualified CabalGild.Unstable.Effect.Warn as Warn
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified Control.Exception as E
import qualified Distribution.Fields as Fields
import Prelude hiding (Read, read)

-- | High level wrapper around 'field' that makes this action easier to compose
-- with other actions.
run ::
  (eX :> es, eR :> es, eW :> es, eWn :> es) =>
  Exception.Exception E.SomeException eX ->
  Read.Read eR ->
  Walk.Walk eW ->
  Warn.Warn eWn ->
  FilePath ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  Eff es ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run ex readH walkH warnH p x =
  WarnUnknown.run warnH x
    >>= EvaluateRequire.run ex
    >>= pure . EvaluateVersion.run
    >>= EvaluateDiscover.run ex walkH p
    >>= EvaluateFragment.run readH warnH p
