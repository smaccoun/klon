module Klon.Command.Connect where

import Data.Text
import Shelly hiding (FilePath)

data ConnectionType = SSH | Tunnel

type MkSSHCmd = FilePath -> Text -> Sh Text

newtype PrivateKeyLoc = PrivateKeyLoc Text

baseSSH_Cmd :: PrivateKeyLoc -> [Text] -> Sh Text
baseSSH_Cmd (PrivateKeyLoc privateKeyLoc) =
    command "ssh" ["-i", privateKeyLoc]

sshCmd ::  Text -> PrivateKeyLoc -> Sh Text
sshCmd serverName' privateKeyLoc = 
    baseSSH_Cmd privateKeyLoc [serverName']

tunnelCmd :: Text -> Text -> PrivateKeyLoc -> Sh Text
tunnelCmd connStr serverName' privateKeyLoc =
    baseSSH_Cmd privateKeyLoc ["-L", connStr, serverName']