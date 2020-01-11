module Klon.CLI.Run where

import Data.Maybe (fromMaybe)
import Klon.Cloud.Resources.AWS.ECS
import Klon.Cloud.Resources.Types
import Klon.Command.Connect
import Klon.Config
import Klon.TUI.TUI (bootTUI)
import Lib
import Shelly

runCLI :: IO ()
runCLI = do
  mbArgs <- captureArgs
  baseConfig <- readDhall "./config.dhall"
  putStrLn $ show baseConfig
  bootProg mbArgs baseConfig

bootProg mbArgs baseConfig =
  case mbArgs of
    Nothing -> do
      inputConfig <- bootTUI
      print $ show inputConfig
      return ()
    Just (Command connectType cli mbAwsProf) -> do
      let awsProf = fromMaybe (_awsProfile baseConfig) mbAwsProf
      awsRunner <- mkRunAWS_ awsProf
      ec2IP <- awsRunner $ getAnEC2InstancePublicIP (mapClusterName cli)
      let server = ServerConnectedToDB ec2IP
          connectCmd = (getSSHCmd connectType server) (PrivateKeyLoc "~/.ssh/id_rsa")
      s <- shelly connectCmd
      print s
  where
    mapClusterName env' = ContainerCluster $
      case env' of
        Staging -> "staging"
        Dev -> "dev"
    localPort = PortToConnect 8888
    dbPort = DBPort 5432
    dbUrl = DatabaseURL "fakeUrl"
    portFowardStr = tunnelForwardArg dbUrl dbPort localPort
    getSSHCmd connectType server =
      case connectType of
        SSH -> sshCmd server
        Tunnel -> tunnelCmd portFowardStr server
