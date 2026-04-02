{-# LANGUAGE TypeOperators #-}

module CabalGild.Unstable.Effect.Write where

import Bluefin.Compound (useImpl)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE)
import qualified Bluefin.IO as IO
import qualified CabalGild.Unstable.Type.Output as Output
import qualified Data.ByteString as ByteString

data Write e = MkWrite
  { writeImpl :: Output.Output -> ByteString.ByteString -> Eff e ()
  }

write :: (e :> es) => Write e -> Output.Output -> ByteString.ByteString -> Eff es ()
write (MkWrite f) o bs = useImpl $ f o bs

makeWriteIO :: IOE e -> Write e
makeWriteIO ioe = MkWrite $ \o bs -> IO.effIO ioe $ case o of
  Output.Stdout -> ByteString.putStr bs
  Output.File f -> ByteString.writeFile f bs
