module Klon.Cloud.Resources.AWS.ECR where

import Data.Git.Monad
import Data.List (sortBy)
import Klon.Config.Types
import Lens.Micro ((.~))
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.ECR.DescribeImages
import Network.AWS.ECR.Types (ImageDetail (..), idImageTags, idImagePushedAt, iiImageTag, imageIdentifier)
import RIO
import RIO.List (headMaybe)
import RIO.List.Partial (head)

class HasDockerImageRepo env where
  dockerImageRepoL :: Lens' env Text

class HasECR_Repo env where
  ecrRepoUriL :: Lens' env Text

instance (HasECR_Repo a) => HasDockerImageRepo a where
  dockerImageRepoL = ecrRepoUriL

instance HasECR_Repo ServiceSpec where
  ecrRepoUriL = remoteImageRepo

dockerImageLoc = undefined

pullImage = undefined

deployImage = undefined

getLastNStoredImages :: (MonadAWS m, WithService m) => Text -> Int -> m [ImageDetail]
getLastNStoredImages serviceName numImages = do
  service <- forService serviceName
  allImgs <- fetchImages (service ^. remoteImageRepo)
  return (allImgs ^. dirsImageDetails)
  where
    fetchImages repo' = 
      Aws.send $
        describeImages repo'
          & (diMaxResults .~ Just (fromIntegral numImages))

getImageForCommit :: (MonadAWS m, WithService m) => Text -> Text -> m (Maybe ImageDetail)
getImageForCommit repo' tag' = do
  matchedImgResp <- fetchByImageTag
  return $ headMaybe $ matchedImgResp ^. dirsImageDetails
  where
    imageTagId = imageIdentifier & iiImageTag .~ (Just tag')
    fetchByImageTag =
      Aws.send $
        describeImages repo'
          & (diImageIds .~ [imageTagId])


imageForCurrentCommit :: (MonadAWS m, GitMonad m, WithService m) => Text -> m (Maybe ImageDetail)
imageForCurrentCommit repoName' = do
  curCommit <- headResolv
  case curCommit of
    (Just rsha1) -> getImageForCommit repoName' (tshow rsha1)
    Nothing -> return Nothing

anyServiceImageForCurrentCommit :: (MonadAWS m, GitMonad m, MonadReader env m, HasServiceSpecs env) => m (Maybe ImageDetail)
anyServiceImageForCurrentCommit = do
  services <- view serviceSpecsL
  let firstService = head services
  imageForCurrentCommit (firstService ^. remoteImageRepo)


