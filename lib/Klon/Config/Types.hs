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
import RIO.List

data AppContext
  = AppContext
      { _appAwsEnv           :: !Aws.Env,
        _appLogFunc          :: !LogFunc,
        _appServices         :: ![ServiceSpec],
        _ecsDeploymentConfig :: Text
      }
  deriving (Generic)
  deriving anyclass (Show)

makeLenses ''AppContext

class HasServiceSpecs a where
   serviceSpecsL :: Lens' a [ServiceSpec]

instance HasServiceSpecs AppContext where
  serviceSpecsL = appServices

-- instance HasServiceSpecs AppContext where
--   serviceSpecsL = appServices

class Monad m => WithService m where
  forService :: Text -> m ServiceSpec

instance (Monad m, MonadReader env m, HasServiceSpecs env) => WithService m where
  forService serviceName' = do
    services <- view serviceSpecsL
    let matchService s = s ^. serviceName == serviceName'
        mbMatch = RIO.List.find matchService services
    return $ fromMaybe (error "assumed matched service") mbMatch

class HasECS_DeploymentConfig env where
  ecsDeployConfigL :: Lens' env Text

instance HasECS_DeploymentConfig AppContext where
  ecsDeployConfigL = ecsDeploymentConfig