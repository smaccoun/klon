module Klon.Prelude
  ( module M,
    defaultProcConfig,
    runDefaultProc,
  )
where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import Klon.Config.Types.AppContext as M
import Klon.Config.Types.Config as M
import Klon.Monad.Klon as M
import RIO as M
import RIO.Text
import System.Process.Typed

runDefaultProc :: ProcAction -> KlonM ()
runDefaultProc procParts = do
  let bin = (unpack $ procParts ^. procBin)
      args' = (fmap unpack $ procParts ^. procArgs)
      fullProc' = proc bin args'
  (out, err) <- readProcess_ $ defaultProcConfig fullProc'
  logInfo $ display $ decodeUtf8 $ LBS.toStrict out
  return ()

defaultProcConfig proc' =
  setStdin closed
    $ setStdout byteStringOutput
    $ setStderr byteStringOutput
    $ proc'
