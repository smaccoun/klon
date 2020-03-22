module Klon.Command.Deploy where

import RIO
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.ECR.DescribeImages
import Data.List (sortBy)
import Lens.Micro ((.~))
import Network.AWS.ECR.Types (ImageDetail)

newtype ImageTag = ImageTag Text

class HasDockerImageRepo env where
  dockerImageRepoL :: Lens' env Text

class HasECS_DeploymentConfig env where
  ecsDeployConfigL :: Lens' env Text

class HasECR_Config env where
  ecrRepoL :: Lens' env Text

instance (HasECR_Config a) => HasDockerImageRepo a where
  dockerImageRepoL = ecrRepoL

deployToECS :: forall env m. 
    (MonadAWS m,
    MonadReader env m,
    HasDockerImageRepo env, 
    HasECS_DeploymentConfig env
    ) => ImageTag 
      -> m ()
deployToECS tag' = do
  env' <- ask
  image' <- pullImage $ dockerImageLoc (env' ^. dockerImageRepoL)
  deployImage (env' ^. ecsDeployConfigL) image'

dockerImageLoc = undefined
pullImage = undefined
deployImage = undefined

data RemoteImageConfig =
  RemoteImageConfig
    {repo :: Text
    ,deploymentConfig :: Text
    }

instance HasECR_Config RemoteImageConfig where
  ecrRepoL = lens repo (\x y -> x {repo = y})

getLastNStoredImages :: (MonadAWS m, MonadIO m, MonadReader env m, HasECR_Config env) => Int -> m [ImageDetail]
getLastNStoredImages numImages = do
  repo' <- view ecrRepoL
  allImgs <- fetchImages repo'
  return $ sortBy mostRecent (allImgs ^. dirsImageDetails)
  where
    fetchImages repo' =
      Aws.send $ 
        describeImages repo'
        & (diMaxResults .~ Just (fromIntegral numImages))
    mostRecent = undefined
  
