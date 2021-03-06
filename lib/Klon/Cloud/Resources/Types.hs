module Klon.Cloud.Resources.Types where

import Data.Text

newtype ContainerCluster = ContainerCluster Text
newtype ServerConnectedToDB = ServerConnectedToDB Text
newtype DatabaseURL = DatabaseURL Text
newtype DBPort = DBPort Int