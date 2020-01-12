let AppEnv = <Dev | Test>
let DBConfig : Type =
    {dbPort : Integer
    ,dbHost : Text
    ,dbUser : Text
    ,dbPassword : Text
    }
in
  {AppEnv = AppEnv
  ,DBConfig = DBConfig
  }