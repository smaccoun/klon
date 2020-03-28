module Klon.Cloud.Resources.AWS.ECR where

import RIO
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.ECR.DescribeImages
import Data.List (sortBy)
import Lens.Micro ((.~))
import Lens.Micro.TH
import Network.AWS.ECR.Types (ImageDetail)

class HasDockerImageRepo env where
  dockerImageRepoL :: Lens' env Text

class HasECR_Config env where
  ecrRepoL :: Lens' env Text

instance (HasECR_Config a) => HasDockerImageRepo a where
  dockerImageRepoL = ecrRepoL

dockerImageLoc = undefined
pullImage = undefined
deployImage = undefined

data RemoteImageConfig =
  RemoteImageConfig
    {_repo :: Text
    ,_deploymentConfig :: Text
    } deriving (Generic)

makeLenses ''RemoteImageConfig

instance HasECR_Config RemoteImageConfig where
  ecrRepoL = repo


getLastNStoredImages :: (MonadAWS m, MonadReader env m, HasECR_Config env) => Int -> m [ImageDetail]
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
