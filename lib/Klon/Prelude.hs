module Klon.Prelude
  ( module M,
    defaultProcConfig,
    runDefaultProc,
  )
where

import Klon.Config.Types.AppContext as M
import Klon.Config.Types.Config as M
import Klon.Monad.Klon as M
import System.Process.Typed

runDefaultProc proc' = runProcess_ $ defaultProcConfig proc'

defaultProcConfig proc' =
  setStdin createPipe
    $ setStdout byteStringOutput
    $ setStderr byteStringOutput
    $ proc'
