module Klon.CLI.Args where

import Data.Text (Text)
import GHC.Generics
import Klon.Command.Connect (ConnectionType (..))
import Options.Applicative
import Klon.Config (AppEnv(..))

data Command
  = Command
      { _inputConnType :: ConnectionType,
        _inputAppEnv :: AppEnv,
        _inputAwsProfile :: Maybe Text
      }

opts :: ParserInfo (Maybe Command)
opts = info (fullCmdParser <**> helper) idm

captureArgs :: IO (Maybe Command)
captureArgs = execParser opts

fullCmdParser :: Parser (Maybe Command)
fullCmdParser = optional $
  Command
    <$> connectionCmdParser
    <*> envParser
    <*> awsProfileArgParser

connectionCmdParser :: Parser ConnectionType
connectionCmdParser =
  subparser
    ( command "ssh" (info (pure SSH) (progDesc "Connect via SSH"))
        <> command "tunnel" (info (pure Tunnel) (progDesc "Connect via Tunnel"))
    )

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
