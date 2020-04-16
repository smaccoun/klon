module Klon.Config.RunConfig where

import Data.Text hiding (head)
import Klon.Cloud.Resources.AWS.ECR
import Klon.Command.Deploy
import Lens.Micro
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import RIO
import System.IO

data AppConfig
  = AppConfig
      { _appAwsEnv :: !Aws.Env,
        _appLogFunc :: !LogFunc,
        _ecrConfig :: !RemoteImageConfig,
        _ecsDeploymentConfig :: Text
      }
  deriving (Generic)
  deriving anyclass (Show)

makeLenses ''AppConfig

mkAwsConfig :: Text -> IO Aws.Env
mkAwsConfig awsProfileName = do
  awsCredFile <- credFile
  awsEnv <- Network.AWS.newEnv $ FromFile awsProfileName awsCredFile
  let Auth authEnv' = awsEnv ^. envAuth
  lgr <- newLogger Error stdout
  let envWithLogger = awsEnv & envLogger .~ lgr
  return envWithLogger

mkAppConfig :: Text -> IO AppConfig
mkAppConfig awsProfileName = do
  awsEnv' <- mkAwsConfig awsProfileName
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    return $
      AppConfig
        { _appAwsEnv = awsEnv',
          _appLogFunc = logFunc,
          _ecrConfig = RemoteImageConfig "fakeRepo" "fakeDeployment",
          _ecsDeploymentConfig = "fakeDeployConfig"
        }

instance Aws.HasEnv AppConfig where
  environment = lens _appAwsEnv (\x y -> x {_appAwsEnv = y})

instance HasECR_Config AppConfig where
  ecrRepoL = ecrConfig . repo

instance HasECS_DeploymentConfig AppConfig where
  ecsDeployConfigL = lens _ecsDeploymentConfig (\x y -> x {_ecsDeploymentConfig = y})

instance HasLogFunc AppConfig where
  logFuncL = lens _appLogFunc (\x y -> x {_appLogFunc = y})
