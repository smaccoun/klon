module Klon.Cloud.Resources.AWS.Run where

import Data.Text hiding (head)
import Lens.Micro
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import Network.AWS.Auth (credFile)
import System.IO
import RIO

mkAwsConfig :: forall b. Text -> IO (Aws.Env, Logger)
mkAwsConfig awsProfileName = do
  awsCredFile <- credFile
  awsEnv <- Network.AWS.newEnv $ FromFile awsProfileName awsCredFile
  let Auth authEnv' = awsEnv ^. envAuth
  lgr <- newLogger Error stdout
  return (awsEnv, lgr)

runAWS_IO :: forall a. (Aws.Env, Logger) -> AWS a -> IO a
runAWS_IO (awsEnv, lgr) cmd' =
  runResourceT $ runAWS (awsEnv & envLogger .~ lgr) $
    within NorthVirginia cmd'

class HasAwsLogFunc env where
  awsLogFuncL :: Lens' env Logger

runAWS_RIO :: forall a env. (Aws.HasEnv env, HasAwsLogFunc env) => AWS a -> ReaderT env IO a
runAWS_RIO cmd' = do
  awsEnv' <- view Aws.environment
  lgr <- view awsLogFuncL
  runResourceT $ runAWS (awsEnv' & envLogger .~ lgr) $
    within NorthVirginia cmd'
