let SSHConfig: Type =       
      { _sshPrivateKeyLoc : Text
      , _portForwardLocalPort : Integer
      }
let ServiceSpec: Type =
    {_serviceName : Text
    ,_remoteImageRepo : Text
    } 
in let BaseConfig: Type =       
      { _awsProfile : Text
      , _sshConfig : Optional SSHConfig
      , _serviceSpecs: List ServiceSpec
      } 
in {SSHConfig = SSHConfig
   ,BaseConfig = BaseConfig
   ,ServiceSpec = ServiceSpec
   }
