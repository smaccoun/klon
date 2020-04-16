module Klon.Docker.Compose where

import RIO
import Dhall.Yaml

newtype DhallMakeComposeCmd = DhallMakeComposeCmd Text

writeComposeFile :: Maybe FilePath -> DhallMakeComposeCmd -> Text -> IO ByteString
writeComposeFile mbPath (DhallMakeComposeCmd dhallCmd) tag' =
  dhallToYaml Dhall.Yaml.defaultOptions mbPath (dhallCmd <> " " <> tag')
