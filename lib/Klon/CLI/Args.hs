module Klon.CLI.Args where

import Options.Applicative
import GHC.Generics
import Klon.Command.Connect (ConnectionType(..))

data Env = Production | Staging | Dev

data Command = Command ConnectionType Env

opts :: ParserInfo Command
opts = info (fullCmdParser <**> helper) idm

captureArgs :: IO Command
captureArgs = execParser opts

fullCmdParser :: Parser Command
fullCmdParser = Command <$> connectionCmdParser <*> envParser

connectionCmdParser :: Parser ConnectionType
connectionCmdParser =
  subparser
    (  command "ssh" (info (pure SSH) ( progDesc "Connect via SSH" ))
    <> command "tunnel" (info (pure Tunnel) ( progDesc "Connect via Tunnel" ))
    )

envParser :: Parser Env
envParser =
  subparser
    (  command "staging" (info (pure Staging) ( progDesc "Staging Env" ))
    <> command "dev" (info (pure Dev) (progDesc "Dev Env" ))
    <> command "production" (info (pure Dev) (progDesc "Production Env" ))
    )
