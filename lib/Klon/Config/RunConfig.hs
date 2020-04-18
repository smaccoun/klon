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
import Dhall
import Klon.Config.Types

mkAwsConfig :: Text -> IO Aws.Env
mkAwsConfig awsProfileName = do
  awsCredFile <- credFile
  awsEnv <- Network.AWS.newEnv $ FromFile awsProfileName awsCredFile
  let Auth authEnv' = awsEnv ^. envAuth
  lgr <- newLogger Error stdout
  let envWithLogger = awsEnv & envLogger .~ lgr
  return envWithLogger

loadAppConfig :: MonadIO m => m BaseConfig
loadAppConfig =
  liftIO $ readDhall "./dhall/config.dhall"

mkAppConfig :: IO AppContext
mkAppConfig = do
  baseConfig' <- loadAppConfig
  awsEnv' <- mkAwsConfig (baseConfig' ^. awsProfile)
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    return $
      AppContext
        { _appAwsEnv = awsEnv',
          _appLogFunc = logFunc,
          _appServices = baseConfig' ^. serviceSpecs,
          _ecsDeploymentConfig = "fakeDeployConfig"
        }

readDhall :: Text -> IO BaseConfig
readDhall fp = 
  input auto fp