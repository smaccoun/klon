module Main where

import Klon.CLI.Run
import Klon.Cloud.Resources.AWS.ECS
import Klon.Command.Connect
import Klon.Command.Deploy
import Klon.Config.RunConfig
import Klon.Monad.AWS
import Klon.Monad.Klon
import RIO

main :: IO ()
main = do
  runCLI

dre :: KlonM a -> IO a
dre fm = do
  let isVerbose = False -- TODO: get from the command line instead
  logOptions' <- logOptionsHandle stderr isVerbose
  let logOptions = setLogUseTime True logOptions'
  context <- mkAppContext
  withLogFunc logOptions $ \logFunc' ->
    runKlonM (context logFunc') fm
