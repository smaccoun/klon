    let SSHConfig : Type = 
      { _sshPrivateKeyLoc : Text
      , _portForwardLocalPort : Integer
      }
   let types = ./types.dhall
in let BaseConfig : Type = 
    { _awsProfile : Text
    , _sshConfig : SSHConfig
    }
in let mySSHParams : SSHConfig = 
     { _sshPrivateKeyLoc = "~/.ssh/id_rsa"
     , _portForwardLocalPort = +8888
     }
in let appBaseConfig : BaseConfig = 
    { _awsProfile = "default"
    , _sshConfig = mySSHParams
    }
in
  appBaseConfig