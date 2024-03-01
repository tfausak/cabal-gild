{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CabalGild.Type.PureT where

import qualified CabalGild.Class.MonadLog as MonadLog
import qualified CabalGild.Class.MonadRead as MonadRead
import qualified CabalGild.Class.MonadWalk as MonadWalk
import qualified CabalGild.Class.MonadWrite as MonadWrite
import qualified CabalGild.Type.Input as Input
import qualified CabalGild.Type.Output as Output
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.RWS as RWST
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map

type E = Exception.SomeException

type R =
  ( Input.Input -> Maybe ByteString.ByteString,
    FilePath -> Maybe [FilePath]
  )

type W = [String]

type S = Map.Map Output.Output ByteString.ByteString

type Pure = PureT Identity.Identity

runPure :: Pure a -> R -> S -> (Either E a, S, W)
runPure p r = Identity.runIdentity . RWST.runRWST (ExceptT.runExceptT $ runPureT p) r

newtype PureT m a = PureT
  { runPureT :: ExceptT.ExceptT E (RWST.RWST R W S m) a
  }
  deriving (Applicative, Functor, Monad)

instance (Monad m) => MonadLog.MonadLog (PureT m) where
  logLn = PureT . Trans.lift . RWST.tell . pure

instance (Monad m) => MonadRead.MonadRead (PureT m) where
  read k = do
    f <- PureT . Trans.lift $ RWST.asks fst
    case f k of
      Nothing -> Exception.throwM . userError $ "read: not found: " <> show k
      Just v -> pure v

instance (Monad m) => Exception.MonadThrow (PureT m) where
  throwM = PureT . ExceptT.throwE . Exception.toException

instance (Monad m) => MonadWalk.MonadWalk (PureT m) where
  walk k = do
    f <- PureT . Trans.lift $ RWST.asks snd
    case f k of
      Nothing -> Exception.throwM . userError $ "walk: not found: " <> show k
      Just v -> pure v

instance (Monad m) => MonadWrite.MonadWrite (PureT m) where
  write k = PureT . Trans.lift . RWST.modify . Map.insert k
