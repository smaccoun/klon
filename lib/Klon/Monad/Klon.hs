module Klon.Monad.Klon where

import Control.Monad.Catch
import Data.Text hiding (head)
import Lens.Micro
import Lens.Micro.TH
import Network.AWS
import qualified Network.AWS as Aws
import Network.AWS.Auth (credFile)
import RIO
import System.IO
import RIO.Orphans
import Klon.Config.Types
import Klon.Monad.AWS

type KlonM = RIO AppContext

instance MonadAWS KlonM where
  liftAWS awsm = do
    env <- ask
    runAWS_IO (env ^. appAwsEnv) awsm

runKlonM :: AppContext -> KlonM a -> IO a
runKlonM cfg riok = runRIO cfg riok