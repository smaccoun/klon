module Models where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
|]
