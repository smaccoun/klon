module Klon.Command.Deploy where

import RIO
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.ECR.DescribeImages
import Data.List (sortBy)
import Lens.Micro ((.~))
import Lens.Micro.TH
import Network.AWS.ECR.Types (ImageDetail)
import Klon.Cloud.Resources.AWS.ECR

class HasECS_DeploymentConfig env where
  ecsDeployConfigL :: Lens' env Text

data DeployOptions = LastPushedImage

deploy :: forall env m. 
    (MonadAWS m, MonadFail m,
    MonadReader env m,
    HasDockerImageRepo env,
    HasECR_Config env,
    HasECS_DeploymentConfig env
    ) => DeployOptions
      -> m ()
deploy options = do
  env' <- ask
  img <- getImageToPush options 
  image' <- pullImage img
  deployImage (env' ^. ecsDeployConfigL) image'

getImageToPush :: (MonadAWS m, MonadReader env m, HasECR_Config env, MonadFail m) => DeployOptions -> m ImageDetail
getImageToPush options =
  case options of
    LastPushedImage -> do
      [lastImg] <- getLastNStoredImages 1
      return lastImg


pullAndDeployToECS :: forall env m. 
    (MonadAWS m,
    MonadReader env m,
    HasDockerImageRepo env, 
    HasECS_DeploymentConfig env
    ) => ImageDetail
      -> m ()
pullAndDeployToECS tag' = do
  env' <- ask
  image' <- pullImage $ dockerImageLoc (env' ^. dockerImageRepoL)
  deployImage (env' ^. ecsDeployConfigL) image'