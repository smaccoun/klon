let AppEnv: Type = <Dev | Test | Prod | Staging>
let SSHConfig: Type =       
      { _sshPrivateKeyLoc : Text
      , _portForwardLocalPort : Integer
      }
let ServiceTaskSpec: Type =
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
      , _serviceSpecs: List ServiceTaskSpec
      , _mkServiceSpecCmd : Text
      , _runMigrationCmd : ProcAction
--      , _appEnv : AppEnv
      } 
in {SSHConfig = SSHConfig
   ,BaseConfig = BaseConfig
   ,ServiceTaskSpec = ServiceTaskSpec
   ,ProcAction = ProcAction
   ,AppEnv = AppEnv
   }
