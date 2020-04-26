module Klon.Monad.Klon where

import Control.Monad.Catch
import Data.Git
import Data.Git.Monad (GitMonad (..))
import Data.Text hiding (head)
import Klon.Config.Types
import Klon.Monad.AWS
import Lens.Micro
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import RIO
import RIO.List
import RIO.Orphans
import System.IO

type KlonM = RIO AppContext

instance MonadAWS KlonM where
  liftAWS awsm = do
    env <- ask
    runAWS_IO (env ^. appAwsEnv) awsm

instance GitMonad KlonM where

  getGit = liftIO $ withCurrentRepo return

  liftGit = liftIO

runKlonM :: AppContext -> KlonM a -> IO a
runKlonM cfg riok = runRIO cfg riok
