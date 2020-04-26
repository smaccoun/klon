module Main where

import Klon.CLI.Run
import Klon.Cloud.Resources.AWS.ECS
import Klon.Command.Connect
import Klon.Command.Deploy
import Klon.Config.RunConfig
import Klon.Monad.AWS
import Klon.Monad.Klon

main :: IO ()
main = do
  runCLI
--dre :: KlonM a -> IO a
--dre klm = do
--  cfg <- mkAppContext
--  withLogFunc
--  runKlonM cfg klm
