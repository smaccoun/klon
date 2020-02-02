module Klon.Cloud.Resources.AWS.RDS where

import Control.Monad
import Data.Text (Text)
import GHC.Generics
import Lens.Micro
import Lens.Micro.TH
import Network.AWS hiding (Endpoint (..), eAddress)
import Network.AWS.EC2
import Network.AWS.EC2.Types
import Network.AWS.RDS
import qualified Network.AWS.RDS as RDS
import Network.AWS.RDS.Types
import Network.AWS.EC2.DescribeSecurityGroups

data RDSInstanceInfo
  = RDSInstanceInfo
      { _rdsDBEndpoint :: Text,
        _rdsDBName :: Text,
        _rdsDBSecurityGroupIds :: [Maybe Text]
      }
  deriving (Generic, Show)

makeLenses ''RDSInstanceInfo

allInstanceInfo :: MonadAWS m => m [RDSInstanceInfo]
allInstanceInfo = do
  resp <- send describeDBInstances
  let instances = resp ^. ddbirsDBInstances
  return $ rdsInstanceInfo <$> instances

matchDBIngressSG :: MonadAWS m => RDSInstanceInfo -> m (RDSInstanceInfo, [IPPermission])
matchDBIngressSG i = do
  let filterGroupIds :: [Text] = i ^.. rdsDBSecurityGroupIds.traverse._Just
  allSGResp :: DescribeSecurityGroupsResponse <- send $ describeSecurityGroups & dsgsGroupIds .~ filterGroupIds
  let ec2IGPerms = concat $ allSGResp ^.. dsgrsSecurityGroups.traverse.sgIPPermissions
  return (i, ec2IGPerms)

rdsInstanceInfo :: DBInstance -> RDSInstanceInfo
rdsInstanceInfo adb =
  RDSInstanceInfo
    { _rdsDBEndpoint = adb ^?! diEndpoint . _Just ^?! eAddress . _Just,
      _rdsDBName = adb ^?! diDBName . _Just,
      _rdsDBSecurityGroupIds = adb ^.. diVPCSecurityGroups.traverse.vsgmVPCSecurityGroupId
    }