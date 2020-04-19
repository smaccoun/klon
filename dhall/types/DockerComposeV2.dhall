let Service 
  : Type = 
     {name : Text
     ,image : Text
     }
in let ComposeSpec = 
      {version = "2"
      ,services = List Service
      } 
in 
  {Service = Service
  ,ComposeSpec = ComposeSpec
  }