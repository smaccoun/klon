module Main where

import Lib
import Klon.Command.Connect
import Klon.Cloud.Resources.Types
import Klon.Cloud.Resources.AWS.ECS
import Klon.Config
import Shelly
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  Command connectType cli mbAwsProf <- captureArgs
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
