module CabalGild.Unstable.Action.EvaluatePragmas where

import qualified CabalGild.Unstable.Action.EvaluatePragmas.Discover as EvaluateDiscover
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Version as EvaluateVersion
import qualified CabalGild.Unstable.Class.MonadWalk as MonadWalk
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified Control.Monad.Catch as Exception
import qualified Distribution.Fields as Fields

-- | High level wrapper around 'field' that makes this action easier to compose
-- with other actions.
run ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  FilePath ->
  ([Fields.Field (p, Comments.Comments q)], cs) ->
  m ([Fields.Field (p, Comments.Comments q)], cs)
run p (fs, cs) = (,) <$> traverse (EvaluateDiscover.field p . EvaluateVersion.field) fs <*> pure cs
