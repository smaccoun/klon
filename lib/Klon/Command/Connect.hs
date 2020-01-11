module Klon.Command.Connect where

import Data.Text
import GHC.Generics
import Klon.Cloud.Resources.Types (DBPort (..), DatabaseURL (..), ServerConnectedToDB (..))
import Klon.Config (PortToConnect (..), PrivateKeyLoc (..))
import Shelly hiding (FilePath)

data ConnectionType = SSH | Tunnel deriving (Generic, Eq, Show, Ord)

type MkSSHCmd = FilePath -> Text -> Sh Text

baseSSH_Cmd :: PrivateKeyLoc -> [Text] -> Sh Text
baseSSH_Cmd (PrivateKeyLoc privateKeyLoc) =
  command "ssh" ["-i", privateKeyLoc]

sshCmd :: ServerConnectedToDB -> PrivateKeyLoc -> Sh Text
sshCmd (ServerConnectedToDB serverName') privateKeyLoc =
  baseSSH_Cmd privateKeyLoc [serverName']

tunnelCmd :: TunnelForwardStr -> ServerConnectedToDB -> PrivateKeyLoc -> Sh Text
tunnelCmd (TunnelForwardStr forwardStr) (ServerConnectedToDB serverName') privateKeyLoc =
  baseSSH_Cmd privateKeyLoc ["-L", forwardStr, serverName']

newtype TunnelForwardStr = TunnelForwardStr Text

tunnelForwardArg :: DatabaseURL -> DBPort -> PortToConnect -> TunnelForwardStr
tunnelForwardArg (DatabaseURL dbUrl) (DBPort dbPort) (PortToConnect connectPort) =
  TunnelForwardStr $ (pack $ show dbPort) <> ":" <> dbUrl <> ":" <> (pack $ show dbPort)
