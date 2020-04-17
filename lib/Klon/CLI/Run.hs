{-# LANGUAGE AllowAmbiguousTypes #-}

module Klon.CLI.Run where

import Data.Maybe (fromMaybe)
import Klon.Cloud.Resources.AWS.ECS
import Klon.Cloud.Resources.AWS.SSM
import Klon.Cloud.Resources.Types
import Klon.Command.Connect
import Klon.Config.Config
import Klon.Config.RunConfig (mkAwsConfig)
import Klon.TUI.TUI (bootTUI)
import Lens.Micro
import Lib
import Shelly
import Data.Proxy
import Klon.Monad.AWS
import Klon.Cloud.Resources.AWS.RDS
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

runCLI :: IO ()
runCLI = do
  mbArgs <- captureArgs
  baseConfig <- readDhall "./dhall/cloudConnect.dhall"
  putStrLn $ show baseConfig
  bootProg mbArgs baseConfig

bootProg :: Maybe Args -> BaseConfig -> IO ()
bootProg mbArgs baseConfig = do
  awsCfg <- mkAwsConfig $ _awsProfile baseConfig
  rdsInfo <- runAWS_IO awsCfg $ allInstanceInfo
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
          ec2IP <- runAWS_IO awsCfg $ getAnEC2InstancePublicIP (mapClusterName appEnv')
          Just pgConf <- runAWS_IO awsCfg $ (fromSSM @PGConf) id
          let server = ServerConnectedToDB ec2IP
              sshConfig' = fromMaybe defaultSSHConfig $ baseConfig ^. sshConfig
              privateKeyLoc = PrivateKeyLoc (sshConfig' ^. sshPrivateKeyLoc) -- TODO
              connectCmd = (getSSHCmd pgConf connectType server) privateKeyLoc
          s <- shelly connectCmd
          print s
  where
    mapClusterName env' = ContainerCluster $
      case env' of
        Staging -> "staging"
        Dev -> "dev"
    getSSHCmd pgConf connectType server =
      case connectType of
        SSH -> sshCmd server
        Tunnel -> tunnelCmd (portFowardStr pgConf) server

portFowardStr :: PGConf -> TunnelForwardStr
portFowardStr PGConf{..} = 
  let localPort = PortToConnect 8888
      dbPort = DBPort pgPort
      dbUrl = DatabaseURL pgHost
  in tunnelForwardArg dbUrl dbPort localPort

modifyConfigWithFlags :: Flags -> BaseConfig -> BaseConfig
modifyConfigWithFlags flgs baseConfig =
  BaseConfig
    { _awsProfile = fromMaybe (_awsProfile baseConfig) (_inputAwsProfile flgs),
      _sshConfig = baseConfig ^. sshConfig,
      _serviceSpecs = [] -- TODO
    }


data PGConf
  = PGConf
      { pgPort :: Int,
        pgUser :: Text,
        pgDb :: Text,
        pgPass :: Text,
        pgHost :: Text
      }
  deriving (Generic, FromJSON, Show, FromSSM)