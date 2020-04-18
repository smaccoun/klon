module Klon.Config.Config where

import Data.Functor.Identity (Identity (..))
import Data.Text
import GHC.Generics
import Lens.Micro.TH
import Dhall
import Dhall.TH

Dhall.TH.makeHaskellTypes
  [ SingleConstructor  "SSHConfig" "SSHConfig" "(./dhall/types/BaseConfigTypes.dhall).SSHConfig"
  , SingleConstructor  "ServiceSpec" "ServiceSpec" "(./dhall/types/BaseConfigTypes.dhall).ServiceSpec"
  , SingleConstructor  "BaseConfig" "BaseConfig" "(./dhall/types/BaseConfigTypes.dhall).BaseConfig"
  ]

deriving instance Show SSHConfig
deriving instance Show ServiceSpec
deriving instance Show BaseConfig

data AppEnv = Production | Staging | Dev deriving (Generic, Show, Eq, Ord)

makeLenses ''ServiceSpec

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
instance FromDhall PrivateKeyLoc
instance FromDhall PortToConnect