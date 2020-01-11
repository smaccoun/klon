let AppEnv = <Dev | Staging | Production>
in
  { _appEnv = AppEnv.Staging
  , _awsProfile = "default"
  }