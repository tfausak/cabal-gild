{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Effect.Read where

import Bluefin.Compound (useImpl)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE)
import qualified Bluefin.IO as IO
import qualified CabalGild.Unstable.Type.Input as Input
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import Prelude hiding (Read, read)

data Read e = MkRead
  { readImpl :: Input.Input -> Eff e ByteString.ByteString,
    tryReadImpl :: Input.Input -> Eff e (Either Exception.SomeException ByteString.ByteString)
  }

read :: (e :> es) => Read e -> Input.Input -> Eff es ByteString.ByteString
read (MkRead f _) i = useImpl $ f i

tryRead :: (e :> es) => Read e -> Input.Input -> Eff es (Either Exception.SomeException ByteString.ByteString)
tryRead (MkRead _ f) i = useImpl $ f i

makeReadIO :: IOE e -> Read e
makeReadIO ioe = MkRead
  { readImpl = \i -> IO.effIO ioe $ case i of
      Input.Stdin -> ByteString.getContents
      Input.File f -> ByteString.readFile f,
    tryReadImpl = \i -> IO.effIO ioe $ Exception.try $ case i of
      Input.Stdin -> ByteString.getContents
      Input.File f -> ByteString.readFile f
  }
