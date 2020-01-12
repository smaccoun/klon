   let pgPort : Integer = +5432
   let types = ./types.dhall
in let AppEnv = types.AppEnv
   let dbConfig = types.DBConfig
in let envToConfigMap =
     {Dev = 
        {dbPort = pgPort 
        ,dbHost = "localhost"
        ,dbUser = "postgres"
        ,dbPassword = "password"
        }
     ,Test = 
        {dbPort = pgPort 
        ,dbHost = "localhost"
        ,dbUser = "postgres"
        ,dbPassword = "password"
        }
     }
in \(env : AppEnv) ->
  merge envToConfigMap env
     
