module Main where

import Klon.CLI.Run
import Klon.Cloud.Resources.AWS.ECS
import Klon.Monad.AWS
import Klon.Monad.Klon
import Klon.Command.Connect
import Klon.Command.Deploy
import Klon.Config.RunConfig

main :: IO ()
main = do
  runCLI

dre :: KlonM a -> IO a
dre klm = do
  cfg <- mkAppConfig
  runKlonM cfg klm
