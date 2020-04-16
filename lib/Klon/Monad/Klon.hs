module Klon.Monad.Klon where

import Control.Monad.Catch
import Data.Text hiding (head)
import Klon.Cloud.Resources.AWS.ECR
import Klon.Command.Deploy
import Klon.Config.RunConfig
import Lens.Micro
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import RIO
import System.IO
import Klon.Monad.AWS
import RIO.Orphans

type KlonM = RIO AppConfig

instance MonadAWS KlonM where
  liftAWS awsm = do
    env <- ask
    runAWS_IO (env ^. appAwsEnv) awsm

runKlonM :: AppConfig -> KlonM a -> IO a
runKlonM cfg riok = runRIO cfg riok