module Main where

import Lib
import Klon.Command.Connect
import Klon.Cloud.Resources.Types
import Shelly

main :: IO ()
main = do
  Command connectType cli <- captureArgs
  let server = ServerConnectedToDB "fakeServer" 
      connectCmd = (getSSHCmd connectType server) (PrivateKeyLoc "~/.ssh/id_rsa")
  s <- shelly connectCmd
  print s
  where
      localPort = PortToConnect 8888
      dbPort = DBPort 5432
      dbUrl = DatabaseURL "fakeUrl"
      portFowardStr = tunnelForwardArg dbUrl dbPort localPort
      getSSHCmd connectType server = 
        case connectType of
            SSH -> sshCmd server 
            Tunnel -> tunnelCmd portFowardStr server
