module Klon.Monad.AWS where

-- import Control.Monad.Freer
-- import Control.Monad.Freer.Reader
-- import Control.Monad.Freer.TH

import Klon.Cloud.Resources.AWS.ECR
import Klon.Config.Types
import Lens.Micro
import qualified Network.AWS as Aws
import RIO

runAWS_IO :: MonadUnliftIO m => Aws.Env -> Aws.AWS a -> m a
runAWS_IO awsEnv cmd' =
  Aws.runResourceT $ Aws.runAWS (awsEnv & Aws.envLogger .~ lgr) $
    Aws.within Aws.NorthVirginia cmd'
  where
    lgr = awsEnv ^. Aws.envLogger

runAWS_RIO :: (Aws.HasEnv env, HasECR_Config env, MonadUnliftIO m, MonadReader env m) => Aws.AWS a -> m a
runAWS_RIO cmd' = do
  awsEnv' <- view Aws.environment
  runAWS_IO awsEnv' cmd'

-- data AWS a where
--   SendAWS :: (Aws.AWSRequest a) => a -> AWS (Aws.Rs a)

-- makeEffect ''AWS

-- runAWS ::
--   Members '[Reader AppConfig, IO] effs =>
--   Eff (AWS ': effs) r ->
--   Eff effs r
-- runAWS = interpret $ \awsEff ->
--   case awsEff of
--     SendAWS req' -> do
--       env' <- ask
--       send $
--         runAWS_IO
--           (env' ^. appAwsEnv)
--           (Aws.send req')
