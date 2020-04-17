module Klon.Command.Deploy where

import Data.List (sortBy)
import Klon.Cloud.Resources.AWS.ECR
import Klon.Monad.Klon
import Klon.Config.Types
import Lens.Micro ((.~))
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.ECR.DescribeImages
import Network.AWS.ECR.Types (ImageDetail)
import RIO

data DeployOptions = LastPushedImage

deploy :: DeployOptions ->
  KlonM ()
deploy options = do
  env' <- ask
  img <- imageForCurrentCommit "app" 
  image' <- pullImage img
  deployImage (env' ^. ecsDeployConfigL) image'