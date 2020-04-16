module Klon.Config.Config where

import Data.Functor.Identity (Identity (..))
import Data.Text
import GHC.Generics
import Lens.Micro.TH
import Dhall

data AppEnv = Production | Staging | Dev deriving (Generic, Show, Eq, Ord)

data BaseConfig
  = BaseConfig
      { _awsProfile :: Text
      , _sshConfig :: SSHConfig
      }
  deriving (Generic, Show)

data SSHConfig
  = SSHConfig
      { _sshPrivateKeyLoc :: Text
      , _portForwardLocalPort :: Integer
      }
  deriving (Generic, Show)

newtype PrivateKeyLoc = PrivateKeyLoc {f_PrivateKeyLoc :: Text} deriving (Generic, Show)
newtype PortToConnect = PortToConnect {f_PortToConnect :: Integer} deriving (Generic, Show)

defaultSSHConfig :: SSHConfig
defaultSSHConfig = SSHConfig
  { _sshPrivateKeyLoc = "~/.ssh/id_rsa",
    _portForwardLocalPort = 8888
  }

makeLenses ''BaseConfig

makeLenses ''SSHConfig

{- DHALL -}

readDhall :: Text -> IO BaseConfig
readDhall fp = 
  input auto fp 

instance FromDhall AppEnv
instance FromDhall BaseConfig
instance FromDhall PrivateKeyLoc
instance FromDhall PortToConnect
instance FromDhall SSHConfig