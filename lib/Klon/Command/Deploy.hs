module Klon.Command.Deploy where

import Data.Git.Monad
import Data.List (sortBy)
import Klon.Cloud.Resources.AWS.ECR
import Klon.Config.Types
import Klon.Docker.Compose
import Klon.Monad.Klon
import Lens.Micro ((.~))
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.ECR.DescribeImages
import Network.AWS.ECR.Types (ImageDetail (..), idImageTags)
import Options.Applicative
import RIO

data DeployOptions = LastPushedImage

data DeploySubCmd = UpdateServiceCmd | MkSpecFileCmd

parseDeployCmd :: Parser DeploySubCmd
parseDeployCmd = subparser $ updateServiceCmd <> mkSpecFileCmd
  where
    updateServiceCmd =
      command "updateService" (info (pure UpdateServiceCmd) (progDesc "Rolling ECR deployment"))
    mkSpecFileCmd =
      command "mkSpec" (info (pure MkSpecFileCmd) (progDesc "Intermediate cmd. Write the compose file required for deployment"))

runDeployCmd :: DeploySubCmd -> KlonM ()
runDeployCmd cmd = case cmd of
  UpdateServiceCmd -> undefined
  MkSpecFileCmd -> do
    mbImageExists <- anyServiceImageForCurrentCommit
    case mbImageExists of
      Just img -> do
        dhallCmd <- view (baseConfigL . mkServiceSpecCmd)
        let tag' = (head $ img ^. idImageTags)
        writeComposeFile Nothing (DhallMakeComposeCmd dhallCmd) tag'
      Nothing ->
        liftIO $ print "No image has been pushed for the current commit"

deploy ::
  DeployOptions ->
  KlonM ()
deploy options = do
  env' <- ask
  img <- imageForCurrentCommit "app"
  image' <- pullImage img
  deployImage (env' ^. ecsDeployConfigL) image'
