module Klon.CLI.Args where

import Data.Text (Text)
import GHC.Generics
import Klon.Command.Connect (ConnectionType (..))
import Klon.Command.Deploy (DeploySubCmd (..), parseDeployCmd)
import Klon.Command.Query (QueryCmd (..), querySubCmdParser)
import Klon.Config (AppEnv (..))
import Options.Applicative

data Command
  = Connect ConnectionCmd
  | QueryState QueryCmd
  | DeployCmd DeploySubCmd

data ConnectionCmd = ConnectionCmd ConnectionType AppEnv

data Flags
  = Flags
      { _inputAwsProfile :: Maybe Text
      }

data Args
  = Args
      { _command :: Command,
        _flags :: Flags
      }

opts :: ParserInfo (Maybe Args)
opts = info (fullParser <**> helper) idm

captureArgs :: IO (Maybe Args)
captureArgs = execParser opts

fullParser :: Parser (Maybe Args)
fullParser =
  optional $
    Args
      <$> cmdParser
      <*> flagParser

cmdParser :: Parser Command
cmdParser =
  (Connect <$> connectCmdParser)
    <|> (QueryState <$> queryStateParser)
    <|> (DeployCmd <$> parseDeployCmd)

queryStateParser :: Parser QueryCmd
queryStateParser =
  subparser $ command "query" queryParseInfo
  where
    queryParseInfo =
      info (querySubCmdParser <**> helper) idm

connectCmdParser :: Parser ConnectionCmd
connectCmdParser =
  ConnectionCmd
    <$> connTypeParser <*> appEnvArgParser

appEnvArgParser :: Parser AppEnv
appEnvArgParser =
  subparser
    ( command "dev" (info (pure Dev) (progDesc "Dev Env"))
        <> command "staging" (info (pure Staging) (progDesc "Staging Env"))
    )

connTypeParser :: Parser ConnectionType
connTypeParser =
  subparser
    ( command "ssh" (info (pure SSH) (progDesc "Connect via SSH"))
        <> command "tunnel" (info (pure Tunnel) (progDesc "Connect via Tunnel"))
    )

flagParser :: Parser Flags
flagParser =
  Flags <$> awsProfileArgParser

envParser :: Parser AppEnv
envParser =
  subparser
    ( command "staging" (info (pure Staging) (progDesc "Staging AppEnv"))
        <> command "dev" (info (pure Dev) (progDesc "Dev AppEnv"))
        <> command "production" (info (pure Dev) (progDesc "Production AppEnv"))
    )

awsProfileArgParser :: Parser (Maybe Text)
awsProfileArgParser =
  optional $ strOption (long "aws-profile")
