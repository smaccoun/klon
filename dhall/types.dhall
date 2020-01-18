   let AppEnv = <Dev | Test>
   let ParamValue = <AsText : Text | AsInt : Integer>
in let Param = {paramName : Text,  paramValue : ParamValue}
in let ParamSource = <Lit : Param | RemoteParam : ParamValue>
in 
  let DBConfig : Type =
        {dbPort : Integer
        ,dbHost : Text
        ,dbUser : Text
        ,dbName : Text
        ,dbPassword : Text
        }
in
  {AppEnv = AppEnv
  ,DBConfig = DBConfig
  ,Param = Param
  ,ParamSource = ParamSource
  ,ParamValue = ParamValue
  }