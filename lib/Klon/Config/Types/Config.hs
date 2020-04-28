module Klon.Config.Types.Config where

import Data.Functor.Identity (Identity (..))
import Data.Text
import Dhall
import Dhall.TH
import GHC.Generics
import Lens.Micro.TH

Dhall.TH.makeHaskellTypes
  [ SingleConstructor "SSHConfig" "SSHConfig" "(./dhall/types/BaseConfigTypes.dhall).SSHConfig",
    SingleConstructor "ServiceTaskSpec" "ServiceTaskSpec" "(./dhall/types/BaseConfigTypes.dhall).ServiceTaskSpec",
    --    MultipleConstructors "AppEnv"  "(./dhall/types/BaseConfigTypes.dhall).AppEnv",
    SingleConstructor "ProcAction" "ProcAction" "(./dhall/types/BaseConfigTypes.dhall).ProcAction",
    SingleConstructor "BaseConfig" "BaseConfig" "(./dhall/types/BaseConfigTypes.dhall).BaseConfig"
  ]

deriving instance Show SSHConfig

deriving instance Show ServiceTaskSpec

deriving instance Show ProcAction

deriving instance Show BaseConfig

makeLenses ''ServiceTaskSpec

makeLenses ''ProcAction

makeLenses ''BaseConfig

makeLenses ''SSHConfig

data AppEnv = Production | Staging | Dev deriving (Generic, Show, Eq, Ord)

newtype PrivateKeyLoc = PrivateKeyLoc {f_PrivateKeyLoc :: Text} deriving (Generic, Show)

newtype PortToConnect = PortToConnect {f_PortToConnect :: Integer} deriving (Generic, Show)

defaultSSHConfig :: SSHConfig
defaultSSHConfig = SSHConfig
  { _sshPrivateKeyLoc = "~/.ssh/id_rsa",
    _portForwardLocalPort = 8888
  }

{- DHALL -}

instance FromDhall AppEnv

instance FromDhall PrivateKeyLoc

instance FromDhall PortToConnect
