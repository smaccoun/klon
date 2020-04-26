module Klon.Command.DBMigration where

import Klon.Prelude
import Options.Applicative
import RIO
import RIO.Text
import System.Process.Typed

runMigrationsOnProcHost :: KlonM ()
runMigrationsOnProcHost = do
  cmdToRun <- view $ baseConfigL . runMigrationCmd
  logInfo $ display $ "Running migrations using: " <> (displayProcAction cmdToRun)
  runDefaultProc cmdToRun
  where
    displayProcAction pa =
      (pa ^. procBin) <> " " <> (intercalate " " $ pa ^. procArgs)

-- runMigrationOnRemote :: KlonM ()
-- runMigrationOnRemote = given [EnvMatters] $ do
--   curEnv' <-
--   locateResource appEnv [CanRunMigration]

data DBMigrateSubCmd = MigrateUpCmd

parseDBMigrateSubCmd :: Parser DBMigrateSubCmd
parseDBMigrateSubCmd =
  subparser $
    migrateUpCmd
  where
    migrateUpCmd =
      command "migrateUp" (info (pure MigrateUpCmd) (progDesc "Migrate DB up"))
