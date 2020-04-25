module Klon.Command.DBMigration where

import Klon.Prelude
import Options.Applicative
import RIO
import RIO.Text
import System.Process.Typed

runMigrationsOnProcHost :: KlonM ()
runMigrationsOnProcHost = do
  cmdToRun <- view $ baseConfigL . runMigrationCmd
  runDefaultProc $ shell $ unpack cmdToRun

data DBMigrateSubCmd = MigrateUpCmd

parseDBMigrateSubCmd :: Parser DBMigrateSubCmd
parseDBMigrateSubCmd =
  subparser $
    migrateUpCmd
  where
    migrateUpCmd =
      command "migrateUp" (info (pure MigrateUpCmd) (progDesc "Migrate DB up"))
