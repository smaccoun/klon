module Klon.Cloud.Resources.AWS.RDS where

import Data.Text (Text)
import Lens.Micro
import Network.AWS hiding (Endpoint (..), eAddress)
import Network.AWS.RDS
import qualified Network.AWS.RDS as RDS
import Network.AWS.RDS.Types

allInstanceInfo :: MonadAWS m => m [RDSInstanceInfo]
allInstanceInfo = do
  resp <- send describeDBInstances
  let instances = resp ^. ddbirsDBInstances
  return $ rdsInstanceInfo <$> instances


rdsInstanceInfo :: DBInstance -> RDSInstanceInfo
rdsInstanceInfo adb =
    RDSInstanceInfo
      { _rdsDBEndpoint = adb ^?! diEndpoint . _Just ^?! eAddress . _Just,
        _rdsDBName = adb ^?! diDBName . _Just,
        _rdsDBSecuritygGroupName = adb ^.. diDBSecurityGroups . traverse . dsgmDBSecurityGroupName
      }

data RDSInstanceInfo
  = RDSInstanceInfo
      { _rdsDBEndpoint :: Text,
        _rdsDBName :: Text,
        _rdsDBSecuritygGroupName :: [Maybe Text]
      }
