module Klon.Cloud.Resources.AWS.Run where

import Data.Text hiding (head)
import Lens.Micro
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import Network.AWS.Auth (credFile)
import System.IO
import RIO
import Klon.Command.Deploy
import Klon.Cloud.Resources.AWS.ECR

data AppConfig =
  AppConfig
    {_appAwsEnv  :: !Aws.Env
    ,_appLogFunc :: !LogFunc
    ,_ecrConfig  :: !RemoteImageConfig
    } deriving (Generic)

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
        {_appAwsEnv = awsEnv'
        ,_appLogFunc = logFunc
        ,_ecrConfig = RemoteImageConfig "fakeRepo" "fakeDeployment"
        }


instance Aws.HasEnv AppConfig where
  environment = lens _appAwsEnv (\x y -> x {_appAwsEnv = y})

instance HasECR_Config AppConfig where
  ecrRepoL = ecrConfig.repo

instance HasLogFunc AppConfig where
  logFuncL = lens _appLogFunc (\x y -> x {_appLogFunc = y})

class Monad m => MonadApp m where
  runApp :: AppConfig -> m a -> IO a

instance MonadApp AWS where
  runApp cfg = runAWS_IO (_appAwsEnv cfg)

instance MonadApp (RIO AppConfig) where
  runApp = runRIO

instance MonadApp (ReaderT AppConfig AWS) where
  runApp cfg m = runApp cfg $ runReaderT m cfg

runAWS_RIO :: (Aws.HasEnv env, HasECR_Config env, MonadUnliftIO m, MonadReader env m) => AWS a -> m a
runAWS_RIO cmd' = do
  awsEnv' <- view Aws.environment
  runResourceT $ runAWS awsEnv' $
    within NorthVirginia cmd'


runAWS_IO :: Aws.Env -> AWS a -> IO a
runAWS_IO awsEnv cmd' =
  runResourceT $ runAWS (awsEnv & envLogger .~ lgr) $
    within NorthVirginia cmd'
  where
    lgr = awsEnv ^. Aws.envLogger