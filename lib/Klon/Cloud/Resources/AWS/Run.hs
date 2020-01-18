module Klon.Cloud.Resources.AWS.Run where

import Data.Text hiding (head)
import Lens.Micro
import Network.AWS
import Network.AWS
import Network.AWS.Auth (credFile)
import Network.AWS.Auth (credFile)
import System.IO

mkAwsConfig :: forall b. Text -> IO (Env, Logger)
mkAwsConfig awsProfileName = do
  awsCredFile <- credFile
  awsEnv <- Network.AWS.newEnv $ FromFile awsProfileName awsCredFile
  let Auth authEnv' = awsEnv ^. envAuth
  lgr <- newLogger Error stdout
  return (awsEnv, lgr)

runAWS_IO :: forall a. (Env, Logger) -> AWS a -> IO a
runAWS_IO (awsEnv, lgr) cmd' =
  runResourceT $ runAWS (awsEnv & envLogger .~ lgr) $
    within NorthVirginia cmd'
