let SSHConfig =       
      { _sshPrivateKeyLoc : Text
      , _portForwardLocalPort : Integer
      }
in let BaseConfig: Type =       
      { _awsProfile : Text
      , _sshConfig : Optional SSHConfig
      , _imageRepo : Text
      } 
in let mySSHConfig: SSHConfig = 
     {_sshPrivateKeyLoc = "fake"
     , _portForwardLocalPort = +7878
     }
in 
  {_awsProfile = "costar"
  ,_sshConfig = Some mySSHConfig
  ,_imageRepo = "meowRepo"
  } : BaseConfig