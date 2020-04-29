module Main where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sql (toSqlKey)
import Models
import RIO
import RIO.Orphans

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn $ show (toSqlKey 1 :: PersonId)
  runSimpleApp $ do
    withPostgresqlPool pgUri 1 $ \pool' -> do
      let rsql = flip runSqlPool pool'
      rsql $ runMigration migrateAll
  where
    pgUri = "postgres://postgres@localhost:5432/myapp"
