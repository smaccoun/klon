module Klon.Docker.Compose where

import RIO
import qualified Dhall.Yaml as DYaml 
import Klon.Prelude

newtype DhallMakeComposeCmd = DhallMakeComposeCmd Text

writeComposeFile :: Maybe FilePath -> DhallMakeComposeCmd -> Text -> IO ByteString
writeComposeFile mbPath (DhallMakeComposeCmd dhallCmd) tag' =
  DYaml.dhallToYaml DYaml.defaultOptions mbPath (dhallCmd <> " " <> tag')
