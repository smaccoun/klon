module Klon.Cloud.Resources.AWS.ECS where

import Control.Monad (forM)
import Klon.Cloud.Resources.AWS.RDS
import Klon.Cloud.Resources.Types
import Klon.Config.Types
import Klon.Monad.Klon
import Lens.Micro
import Network.AWS
import Network.AWS
import Network.AWS.Auth (credFile)
import Network.AWS.Auth (credFile)
import Network.AWS.EC2
import Network.AWS.EC2.Types
import Network.AWS.ECS
import Network.AWS.RDS.Types
import RIO
import RIO.ByteString.Lazy (toStrict)
import RIO.List (find, headMaybe)
import RIO.Partial
import qualified RIO.Text as T
import System.IO
import System.Process.Typed

-- getAnEC2InstanceConnectedToDB :: MonadAWS m => (DBInstance, [EC2SecurityGroup]) -> m [Instance]
-- getAnEC2InstanceConnectedToDB db = do
--   cresp <- send describeClusters
--   let clusterNames = cresp ^.. dcrsClusters . traverse . cClusterName . _Just
--   forM clusterNames $ \cn -> do
--     ec2Instances' <- getEC2_InstancesInCluster [cn]
--     return $ ec2Instances `whenHasMatchingSecurityGroup` db
--   where
--     ec2SGNames ss = Prelude.concat $
--       fmap (\ec2I -> (ec2I ^.. insSecurityGroups . traverse . giGroupName . _Just)) ss
--     whenHasMatchingSecurityGroup ec2Instances' (dbi, dEC2_SGs) =
--       Prelude.filter (\ec2SG -> ec2SG `elem` dEC2_SGs) ec2Instances'

getEC2_InstancesInCluster :: MonadAWS m => [Text] -> m [Instance]
getEC2_InstancesInCluster containerInstanceIDs = do
  matchingEC2InstancesResp <- send $ describeInstances & diiInstanceIds .~ containerInstanceIDs
  return $ Prelude.concat $ matchingEC2InstancesResp ^.. dirsReservations . traversed . rInstances

-- | Randomly picks a single ec2 instance inside of a cluster
getAnEC2InstancePublicIP :: MonadAWS m => ContainerCluster -> m Text
getAnEC2InstancePublicIP cluster' = do
  anInstanceID <- getInstanceID_InCluster cluster'
  descInstanceResp <- send $ describeInstances & diiInstanceIds .~ [anInstanceID]
  let [reservation'] = descInstanceResp ^. dirsReservations
      [fullInstanceInfo] = reservation' ^. rInstances
      Just publicIP = fullInstanceInfo ^. insPublicIPAddress
  return publicIP

getInstanceIDs_InCluster :: MonadAWS m => ContainerCluster -> m [Text]
getInstanceIDs_InCluster (ContainerCluster clusterName') = do
  tasksResp <- send $ listTasks & ltCluster .~ (Just clusterName')
  let allTasks = (tasksResp ^.. ltrsTaskARNs . traverse)
  tasksResp <-
    send $
      describeTasks
        & dtTasks .~ allTasks
        & dtCluster .~ (Just clusterName')
  ciResponse <-
    send $
      describeContainerInstances
        & dciCluster .~ (Just clusterName')
        & dciContainerInstances .~ allTasks
  return $ ciResponse ^.. dcisrsContainerInstances . traverse . ciEc2InstanceId . _Just

getInstanceID_InCluster :: MonadAWS m => ContainerCluster -> m Text
getInstanceID_InCluster cluster = do
  ids <- getInstanceIDs_InCluster cluster
  return $ ids ^?! _head

-------------------------

-- | Service Creation
-------------------------
createTaskDef :: FilePath -> KlonM (Maybe Text)
createTaskDef composeFile' = do
  serviceConfig' <- fmap head $ view serviceSpecsL
  let cluster = serviceConfig' ^. clusterName
      projName = serviceConfig' ^. taskProjectName
      ecsCliComposeProc = (procConfig' cluster projName)
  (_, out', _) <- liftIO $ readProcess ecsCliComposeProc
  case (decodeUtf8' $ toStrict out') of
    Right s -> return $ fmap snd $ parseTaskDefResult s
    Left e -> return $ Just $ "Could not decode output" <> (T.pack $ show e)
  where
    procConfig' clusterName' projName' =
      setStdin createPipe
        $ (setStdout byteStringOutput)
        $ (setStderr byteStringOutput)
          (cliComposeProc clusterName' projName')
    cliComposeProc clusterName' projName =
      proc
        "ecs-cli"
        ( ["compose"]
            <> (procFlags clusterName' projName)
            <> procAction
        )
    procAction = ["create"]
    procFlags clusterName' projName' =
      [ "--file",
        composeFile',
        "--cluster",
        (T.unpack clusterName'),
        "--project-name",
        (T.unpack projName')
      ]
    parseOutput out' =
      words (T.unpack out')
        & fmap ((T.split (\a -> a == '=')) . T.pack)
    parseTaskDefResult :: Text -> Maybe (Text, Text)
    parseTaskDefResult out' =
      parseOutput out'
        & catMaybes . fmap parseVarEq
        & find (\(var, _) -> var == "TaskDefinition")
    parseVarEq [var, val] = Just (var, val)
    parseVarEq _ = Nothing
