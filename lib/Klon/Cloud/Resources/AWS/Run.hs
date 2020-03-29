module Klon.Cloud.Resources.AWS.Run where

import Data.Text hiding (head)
import Lens.Micro
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import System.IO
import RIO
import Klon.Command.Deploy
import Klon.Cloud.Resources.AWS.ECR
import Control.Monad.Catch
import Klon.Config.RunConfig

newtype AppM a = 
  AppM {unAppM :: (RIO AppConfig a)}
  deriving anyclass (Generic, Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadFail, MonadThrow, MonadCatch, MonadUnliftIO)

instance MonadAWS AppM where
  liftAWS awsm = do
    env <- ask
    runAWS_IO (env ^. appAwsEnv) awsm

runAWS_RIO :: (Aws.HasEnv env, HasECR_Config env, MonadUnliftIO m, MonadReader env m) => AWS a -> m a
runAWS_RIO cmd' = do
  awsEnv' <- view Aws.environment
  runResourceT $ runAWS awsEnv' $
    within NorthVirginia cmd'

runAWS_IO :: MonadUnliftIO m => Aws.Env -> AWS a -> m a
runAWS_IO awsEnv cmd' =
  runResourceT $ runAWS (awsEnv & envLogger .~ lgr) $
    within NorthVirginia cmd'
  where
    lgr = awsEnv ^. Aws.envLogger