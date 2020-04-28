module Klon.Config.Types.AppContext where

import Data.Text hiding (head)
import Dhall
import Klon.Config.Types.Config
import Lens.Micro
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import RIO
import RIO.List
import System.IO

data AppContext
  = AppContext
      { _appAwsEnv :: !Aws.Env,
        _appLogFunc :: !LogFunc,
        _appServices :: ![ServiceTaskSpec],
        _appBaseConfig :: BaseConfig,
        _ecsDeploymentConfig :: Text
      }
  deriving (Generic)
  deriving anyclass (Show)

makeLenses ''AppContext

instance Aws.HasEnv AppContext where
  environment = lens _appAwsEnv (\x y -> x {_appAwsEnv = y})

instance HasLogFunc AppContext where
  logFuncL = lens _appLogFunc (\x y -> x {_appLogFunc = y})

class HasECS_DeploymentConfig env where
  ecsDeployConfigL :: Lens' env Text

instance HasECS_DeploymentConfig AppContext where
  ecsDeployConfigL = ecsDeploymentConfig

class HasBaseConfig a where
  baseConfigL :: Lens' a BaseConfig

instance HasBaseConfig AppContext where
  baseConfigL = appBaseConfig

class HasServiceSpecs a where
  serviceSpecsL :: Lens' a [ServiceTaskSpec]

instance HasServiceSpecs AppContext where
  serviceSpecsL = appServices

class Monad m => WithService m where
  forService :: Text -> m ServiceTaskSpec

instance (Monad m, MonadReader env m, HasServiceSpecs env) => WithService m where
  forService serviceName' = do
    services <- view serviceSpecsL
    let matchService s = s ^. serviceName == serviceName'
        mbMatch = RIO.List.find matchService services
    return $ fromMaybe (error "assumed matched service") mbMatch
