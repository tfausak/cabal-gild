module CabalGild.Unstable.Action.EvaluatePragmas where

import qualified CabalGild.Unstable.Action.EvaluatePragmas.Discover as EvaluateDiscover
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Fragment as EvaluateFragment
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Require as EvaluateRequire
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Version as EvaluateVersion
import qualified CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown as WarnUnknown
import qualified CabalGild.Unstable.Class.MonadRead as MonadRead
import qualified CabalGild.Unstable.Class.MonadWalk as MonadWalk
import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Distribution.Fields as Fields

-- | High level wrapper around 'field' that makes this action easier to compose
-- with other actions.
run ::
  (Exception.MonadCatch m, MonadRead.MonadRead m, MonadWalk.MonadWalk m, MonadWarn.MonadWarn m) =>
  FilePath ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  m ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run p =
  WarnUnknown.run
    Monad.>=> EvaluateRequire.run
    Monad.>=> EvaluateVersion.run
    Monad.>=> EvaluateDiscover.run p
    Monad.>=> EvaluateFragment.run p
