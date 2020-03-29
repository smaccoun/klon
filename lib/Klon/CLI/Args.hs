module Klon.CLI.Args where

import Data.Text (Text)
import GHC.Generics
import Klon.Command.Connect (ConnectionType (..))
import Options.Applicative
import Klon.Config.Config (AppEnv(..))

data Command = 
  Connect ConnectionCmd

data ConnectionCmd = ConnectionCmd ConnectionType AppEnv

data Flags =
  Flags
    { _inputAwsProfile :: Maybe Text
    }

data Args
  = Args
      { _command :: Command,
        _flags   :: Flags
      }

opts :: ParserInfo (Maybe Args)
opts = info (fullParser <**> helper) idm

captureArgs :: IO (Maybe Args)
captureArgs = execParser opts

fullParser :: Parser (Maybe Args)
fullParser = optional $
  Args
    <$> cmdParser 
    <*> flagParser

cmdParser :: Parser Command
cmdParser =
  Connect <$> connectCmdParser

connectCmdParser :: Parser ConnectionCmd
connectCmdParser = ConnectionCmd <$>
  connTypeParser <*> appEnvArgParser

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
