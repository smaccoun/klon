module Klon.Docker.Compose where

import Dhall.Map (Map (..))
import Dhall.TH
import qualified Dhall.Yaml as DYaml
import Klon.Prelude
import RIO

-- Dhall.TH.makeHaskellTypes
--   [ MultipleConstructors "StringOrNumber" "(./dhall/types/DockerComposeV3.dhall).StringOrNumber"
--   , MultipleConstructors "ListOrDict" "(./dhall/types/DockerComposeV3.dhall).ListOrDict"
--   , SingleConstructor  "ComposeService" "ComposeService" "(./dhall/types/DockerComposeV3.dhall).Service"
--   ]

Dhall.TH.makeHaskellTypes
  [ SingleConstructor "DockerComposeService" "DockerComposeService" "(./dhall/types/DockerComposeV2.dhall).Service",
    SingleConstructor "DockerComposeSpec" "DockerComposeSpec" "(./dhall/types/DockerComposeV2.dhall).ComposeSpec"
  ]

newtype DhallMakeComposeCmd = DhallMakeComposeCmd Text

writeComposeFile :: Maybe FilePath -> DhallMakeComposeCmd -> Text -> KlonM ByteString
writeComposeFile mbPath (DhallMakeComposeCmd dhallCmd) tag' =
  liftIO $
    DYaml.dhallToYaml DYaml.defaultOptions mbPath (dhallCmd <> " " <> tag')
