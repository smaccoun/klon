let AppEnv: Type = <Dev | Test | Prod | Staging>
let SSHConfig: Type =       
      { _sshPrivateKeyLoc : Text
      , _portForwardLocalPort : Integer
      }
let ServiceSpec: Type =
    {_serviceName : Text
    ,_remoteImageRepo : Text
    ,_clusterName : Text
    ,_taskProjectName : Text
    } 
let ProcAction: Type =
   {_procBin: Text
   ,_procArgs: List Text
   }
in let BaseConfig: Type =       
      { _awsProfile : Text
      , _sshConfig : Optional SSHConfig
      , _serviceSpecs: List ServiceSpec
      , _mkServiceSpecCmd : Text
      , _runMigrationCmd : ProcAction
--      , _appEnv : AppEnv
      } 
in {SSHConfig = SSHConfig
   ,BaseConfig = BaseConfig
   ,ServiceSpec = ServiceSpec
   ,ProcAction = ProcAction
   ,AppEnv = AppEnv
   }
