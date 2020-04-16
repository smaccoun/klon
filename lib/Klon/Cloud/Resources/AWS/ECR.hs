module Klon.Cloud.Resources.AWS.ECR where

import RIO
import RIO.List (headMaybe)
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.ECR.DescribeImages
import Data.List (sortBy)
import Lens.Micro ((.~))
import Lens.Micro.TH
import Network.AWS.ECR.Types (ImageDetail(..), idImageTags)
import Klon.Config.Types

class HasDockerImageRepo env where
  dockerImageRepoL :: Lens' env Text

class HasECR_Config env where
  ecrRepoL :: Lens' env Text

instance (HasECR_Config a) => HasDockerImageRepo a where
  dockerImageRepoL = ecrRepoL

dockerImageLoc = undefined
pullImage = undefined
deployImage = undefined


instance HasECR_Config RemoteImageConfig where
  ecrRepoL = repo


getLastNStoredImages :: (MonadAWS m, MonadReader env m, HasECR_Config env) => Int -> m [ImageDetail]
getLastNStoredImages numImages = do
  repo' <- view ecrRepoL
  allImgs <- fetchImages repo'
  return (allImgs ^. dirsImageDetails)
  where
    fetchImages repo' =
      Aws.send $ 
        describeImages repo'
        & (diMaxResults .~ Just (fromIntegral numImages))

getImageForCommit :: (MonadAWS m, MonadReader env m, HasECR_Config env) => Text -> m (Maybe ImageDetail)
getImageForCommit tag'= do
  mostRecentImgs <- getLastNStoredImages 100
  return $ headMaybe $ mostRecentImgs `whereHasTag` tag'
  where
    whereHasTag :: [ImageDetail] -> Text -> [ImageDetail]
    whereHasTag imgs tagToMatch = 
      filter (\img -> tagToMatch `elem` (img ^. idImageTags)) imgs
      

