    let SSHConfig : Type = 
      { _sshPrivateKeyLoc : Text
      , _portForwardLocalPort : Integer
      }
in let BaseConfig : Type = 
    { _awsProfile : Text
    , _sshConfig : SSHConfig
    }
in let mySSHParams : SSHConfig = 
     { _sshPrivateKeyLoc = "~/.ssh/id_rsa"
     , _portForwardLocalPort = +8888
     }
in
  { _awsProfile = "default"
  , _sshConfig = mySSHParams
  } : BaseConfig