module Main where

import Data.Maybe (fromMaybe)
import Klon.Cloud.Resources.AWS.ECS
import Klon.Cloud.Resources.Types
import Klon.Command.Connect
import Klon.Config
import Klon.TUI.TUI (bootTUI)
import Lib
import Shelly

main :: IO ()
main = do
  mbArgs <- captureArgs
  case mbArgs of
    Nothing -> bootTUI
    Just (Command connectType cli mbAwsProf) -> do
      let awsProf = fromMaybe "default" mbAwsProf
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
