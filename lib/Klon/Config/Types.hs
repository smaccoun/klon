module Klon.Config.Types where

import Data.Text hiding (head)
import Lens.Micro
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import RIO
import System.IO
import Dhall
import Klon.Config.Config

data RemoteImageConfig =
  RemoteImageConfig
    {_repo :: Text
    ,_deploymentConfig :: Text
    } deriving (Generic)

makeLenses ''RemoteImageConfig

data AppContext
  = AppContext
      { _appAwsEnv           :: !Aws.Env,
        _appLogFunc          :: !LogFunc,
        _remoteImageRepo     :: !RemoteImageConfig,
        _ecsDeploymentConfig :: Text
      }
  deriving (Generic)
  deriving anyclass (Show)

makeLenses ''AppContext

