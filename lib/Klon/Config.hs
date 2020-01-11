module Klon.Config where

import Data.Functor.Identity (Identity (..))
import Data.Generic.HKD
import Data.Text
import GHC.Generics
import Lens.Micro.TH
import Dhall

newtype PrivateKeyLoc = PrivateKeyLoc Text

newtype PortToConnect = PortToConnect Int

data AppEnv = Production | Staging | Dev deriving (Generic, Show, Eq, Ord)

data BaseConfig
  = BaseConfig
      { _awsProfile :: Text
      }
  deriving (Generic, Show)

instance Interpret AppEnv
instance Interpret BaseConfig

readDhall :: Text -> IO BaseConfig
readDhall fp = 
  input auto fp 

data Config
  = Config
      { _sshConfig :: SSHConfig
      }
  deriving (Generic)

data SSHConfig
  = SSHConfig
      { _sshPrivateKeyLoc :: PrivateKeyLoc,
        _portForwardLocalPort :: PortToConnect
      }
  deriving (Generic)

type InputSSHConfig = HKD SSHConfig Maybe

defaultSSHConfig :: HKD SSHConfig Identity
defaultSSHConfig = deconstruct SSHConfig
  { _sshPrivateKeyLoc = PrivateKeyLoc "~/.ssh/id_rsa",
    _portForwardLocalPort = PortToConnect 8888
  }

makeLenses ''BaseConfig

makeLenses ''SSHConfig

makeLenses ''Config
