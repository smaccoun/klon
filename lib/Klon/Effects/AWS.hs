module Klon.Effects.AWS where

import qualified Network.AWS as Aws
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Reader
import Klon.Config.RunConfig
import Lens.Micro

data AWS a where
  SendAWS :: (Aws.AWSRequest a) => a -> AWS (Aws.Rs a)
makeEffect ''AWS

runAWS_IO ::  Members '[Reader AppConfig, IO] effs
           => Eff (AWS ': effs) r 
           -> Eff effs r
runAWS_IO = interpret $ \awsEff ->
  case awsEff of
    SendAWS req' -> do
      env' <- ask
      send $ 
        runAWSM_IO 
          (env' ^. appAwsEnv)
          (Aws.send req')


runAWSM_IO :: Aws.Env -> Aws.AWS a -> IO a
runAWSM_IO awsEnv cmd' =
  Aws.runResourceT $ Aws.runAWS (awsEnv & Aws.envLogger .~ lgr) $
    Aws.within Aws.NorthVirginia cmd'
  where
    lgr = awsEnv ^. Aws.envLogger