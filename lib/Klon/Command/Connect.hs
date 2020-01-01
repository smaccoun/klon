module Klon.Command.Connect where

import Data.Text

data ConnectionType = SSH | Tunnel

shellCmd :: ConnectionType -> Text
shellCmd SSH = "ssh"
