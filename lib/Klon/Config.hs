module Klon.Config where

import Data.Text
import GHC.Generics
import Data.Generic.HKD
import Data.Functor.Identity (Identity (..))

newtype PrivateKeyLoc = PrivateKeyLoc Text
newtype PortToConnect = PortToConnect Int

data Config =
    Config
      {_sshConfig :: SSHConfig
      } deriving Generic

data SSHConfig =
    SSHConfig
      {_sshPrivateKeyLoc ::  PrivateKeyLoc
      ,_portForwardLocalPort :: PortToConnect
      } deriving Generic

type InputSSHConfig = HKD SSHConfig Maybe

defaultSSHConfig :: HKD SSHConfig Identity
defaultSSHConfig = deconstruct
    SSHConfig
      {_sshPrivateKeyLoc = PrivateKeyLoc "~/.ssh/id_rsa"
      ,_portForwardLocalPort = PortToConnect 8888
      }