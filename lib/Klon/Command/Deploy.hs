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

deployToECS :: forall env m. 
    (MonadAWS m,
    MonadReader env m,
    HasDockerImageRepo env, 
    HasECS_DeploymentConfig env
    ) => ImageDetail
      -> m ()
deployToECS tag' = do
  env' <- ask
  image' <- pullImage $ dockerImageLoc (env' ^. dockerImageRepoL)
  deployImage (env' ^. ecsDeployConfigL) image'