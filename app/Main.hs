module Main where

import Lib
import Klon.Command.Connect
import Shelly

main :: IO ()
main = do
  Command connectType cli <- captureArgs
  let connectCmd = (getSSHCmd connectType) (PrivateKeyLoc "~/.ssh/id_rsa")
  s <- shelly connectCmd
  print s
  where
      getSSHCmd connectCmd = 
        case connectCmd of
            SSH -> sshCmd "fakeServer"
            Tunnel -> tunnelCmd "fakeServer" "8888:fakeDb:5432"
