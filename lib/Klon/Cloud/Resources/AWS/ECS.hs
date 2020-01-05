module Klon.Cloud.Resources.AWS.ECS where

import Data.Text hiding (head)
import Lens.Micro
import Network.AWS
import Network.AWS.Auth (credFile)
import Network.AWS.EC2
  ( describeInstances,
    diiInstanceIds,
    dirsReservations,
    insPublicIPAddress,
    rInstances,
  )
import Network.AWS.ECS
import Klon.Cloud.Resources.Types (ContainerCluster(..))
import           Network.AWS
import           Network.AWS.Auth (credFile)
import           System.IO

mkRunAWS_ :: Text -> IO (AWS b -> IO b)
mkRunAWS_ awsProfileName = do
  awsCredFile <- credFile
  awsEnv <- Network.AWS.newEnv $ FromFile awsProfileName awsCredFile
  let Auth authEnv' = awsEnv ^. envAuth
  lgr  <- newLogger Error stdout
  return $ runAWSCmd awsEnv lgr
  where
    runAWSCmd awsEnv lgr cmd' =
      runResourceT $ runAWS (awsEnv & envLogger .~ lgr) $
        within NorthVirginia cmd'


-- | Randomly picks a single ec2 instance inside of a cluster
getAnEC2InstancePublicIP :: MonadAWS m => ContainerCluster -> m Text
getAnEC2InstancePublicIP cluster' = do
  anInstanceID <- getInstanceID_InCluster cluster'
  descInstanceResp <- send $ describeInstances & diiInstanceIds .~ [anInstanceID]
  let [reservation'] = descInstanceResp ^. dirsReservations
      [fullInstanceInfo] = reservation' ^. rInstances
      Just publicIP = fullInstanceInfo ^. insPublicIPAddress
  return publicIP

getInstanceID_InCluster :: MonadAWS m => ContainerCluster -> m Text
getInstanceID_InCluster (ContainerCluster clusterName') = do
  tasksResp <- send $ listTasks & ltCluster .~ (Just clusterName')
  let firstTaskARN = head $ tasksResp ^. ltrsTaskARNs
  firstTaskResp <-
    send $
      describeTasks
        & dtTasks .~ [firstTaskARN]
        & dtCluster .~ (Just clusterName')
  let firstTask = head $ firstTaskResp ^. dtrsTasks
      Just containerInstanceARN = firstTask ^. tContainerInstanceARN
  ciResponse <-
    send $
      describeContainerInstances
        & dciCluster .~ (Just clusterName')
        & dciContainerInstances .~ [containerInstanceARN]
  let [containerIntance'] = ciResponse ^. dcisrsContainerInstances
      Just instanceID = containerIntance' ^. ciEc2InstanceId
  return instanceID
