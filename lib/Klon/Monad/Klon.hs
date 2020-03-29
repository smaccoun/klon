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

newtype KlonM a
  = KlonM {unKlonM :: (RIO AppConfig a)}
  deriving anyclass (Generic, Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadFail, MonadThrow, MonadCatch, MonadUnliftIO)

instance MonadAWS KlonM where
  liftAWS awsm = do
    env <- ask
    runAWS_IO (env ^. appAwsEnv) awsm

runKlonM :: AppConfig -> KlonM a -> IO a
runKlonM cfg (KlonM riok) = runRIO cfg riok
