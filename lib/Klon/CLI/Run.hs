module Klon.CLI.Run where

import Data.Maybe (fromMaybe)
import Klon.Cloud.Resources.AWS.ECS
import Klon.Cloud.Resources.Types
import Klon.Command.Connect
import Klon.Config
import Klon.TUI.TUI (bootTUI)
import Lib
import Shelly
import Lens.Micro

runCLI :: IO ()
runCLI = do
  mbArgs <- captureArgs
  baseConfig <- readDhall "./dhall/cloudConnect.dhall"
  putStrLn $ show baseConfig
  bootProg mbArgs baseConfig

bootProg :: Maybe Args -> BaseConfig -> IO ()
bootProg mbArgs baseConfig =
  case mbArgs of
    Nothing -> do
      inputConfig <- bootTUI
      print $ show inputConfig
      return ()
    Just (Args cmd flgs) -> do
      let modifiedConfig = modifyConfigWithFlags flgs baseConfig
          awsProf = _awsProfile modifiedConfig
      case cmd of
        Connect (ConnectionCmd connectType appEnv') -> do
          awsRunner <- mkRunAWS_ awsProf
          ec2IP <- awsRunner $ getAnEC2InstancePublicIP (mapClusterName appEnv')
          let server = ServerConnectedToDB ec2IP
              privateKeyLoc = (PrivateKeyLoc (baseConfig ^. sshConfig ^. sshPrivateKeyLoc))
              connectCmd = (getSSHCmd connectType server) privateKeyLoc
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

modifyConfigWithFlags :: Flags -> BaseConfig -> BaseConfig
modifyConfigWithFlags flgs baseConfig =
  BaseConfig
    { _awsProfile = fromMaybe (_awsProfile baseConfig) (_inputAwsProfile flgs)
    , _sshConfig = fromMaybe (_sshConfig baseConfig) (Just defaultSSHConfig)
    }
