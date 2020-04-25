module Klon.Command.DBMigration where

import Klon.Prelude
import RIO
import RIO.Text
import System.Process.Typed

runMigrationsOnProcHost :: KlonM ()
runMigrationsOnProcHost = do
  cmdToRun <- view $ baseConfigL . runMigrationCmd
  runDefaultProc $ shell $ unpack cmdToRun
