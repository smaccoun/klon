module Klon.Command.Connect where

import Data.Text
import Shelly hiding (FilePath)
import Klon.Cloud.Resources.Types (ServerConnectedToDB(..), DatabaseURL(..), DBPort(..))

data ConnectionType = SSH | Tunnel

type MkSSHCmd = FilePath -> Text -> Sh Text

newtype PrivateKeyLoc = PrivateKeyLoc Text

baseSSH_Cmd :: PrivateKeyLoc -> [Text] -> Sh Text
baseSSH_Cmd (PrivateKeyLoc privateKeyLoc) =
  command "ssh" ["-i", privateKeyLoc]

sshCmd :: ServerConnectedToDB -> PrivateKeyLoc -> Sh Text
sshCmd (ServerConnectedToDB serverName') privateKeyLoc =
  baseSSH_Cmd privateKeyLoc [serverName']

tunnelCmd :: TunnelForwardStr -> ServerConnectedToDB -> PrivateKeyLoc -> Sh Text
tunnelCmd (TunnelForwardStr forwardStr) (ServerConnectedToDB serverName') privateKeyLoc =
  baseSSH_Cmd privateKeyLoc ["-L", forwardStr, serverName']

newtype PortToConnect = PortToConnect Int
newtype TunnelForwardStr = TunnelForwardStr Text

tunnelForwardArg :: DatabaseURL -> DBPort -> PortToConnect -> TunnelForwardStr
tunnelForwardArg (DatabaseURL dbUrl) (DBPort dbPort) (PortToConnect connectPort) =
  TunnelForwardStr $  (pack $ show dbPort) <> ":" <> dbUrl <> ":" <> (pack $ show dbPort)
