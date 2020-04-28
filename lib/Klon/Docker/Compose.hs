module Klon.Docker.Compose where

import Data.Aeson
import Data.Aeson.Types (Parser (..))
import Deriving.Aeson
import qualified Dhall.DhallToYaml.Main
import Dhall.Map (Map (..))
import Dhall.TH
import qualified Dhall.Yaml as DYaml
import GHC.Generics
import Klon.Prelude
import RIO
import RIO.File (writeBinaryFile)
import RIO.List.Partial
import qualified RIO.Map as M

-- Dhall.TH.makeHaskellTypes
--   [ MultipleConstructors "StringOrNumber" "(./dhall/types/DockerComposeV3.dhall).StringOrNumber"
--   , MultipleConstructors "ListOrDict" "(./dhall/types/DockerComposeV3.dhall).ListOrDict"
--   , SingleConstructor  "ComposeService" "ComposeService" "(./dhall/types/DockerComposeV3.dhall).Service"
--   ]

Dhall.TH.makeHaskellTypes
  [ SingleConstructor "DockerComposeSpec" "DockerComposeSpec" "(./dhall/types/DockerComposeV2.dhall).ComposeSpec"
  ]

data DockerComposeService
  = DockerComposeService
      { name :: Text,
        spec :: DockerComposeServiceSpec
      }
  deriving (Generic, Show)

instance ToJSON DockerComposeService where
  toJSON DockerComposeService {..} = toJSON $ M.singleton name spec

data DockerComposeServiceSpec
  = DockerComposeServiceSpec
      { _dcCommand :: Maybe Text,
        _dcImage :: Text
      }
  deriving (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "_dc", CamelToSnake)] DockerComposeServiceSpec

newtype DhallMakeComposeCmd = DhallMakeComposeCmd Text

writeComposeFile :: Maybe FilePath -> DhallMakeComposeCmd -> Text -> KlonM ()
writeComposeFile mbPath (DhallMakeComposeCmd dhallCmd) tag' =
  liftIO $ do
    asBS <- DYaml.dhallToYaml DYaml.defaultOptions mbPath (dhallCmd <> " " <> "\"" <> tag' <> "\"")
    writeBinaryFile "./dc.yml" asBS
  where
    options = DYaml.defaultOptions {DYaml.quoted = False}

defaultServantService :: Text -> Text -> KlonM DockerComposeService
defaultServantService bin tag = do
  allServiceSpecs <- view (baseConfigL . serviceSpecs)
  let repo = head allServiceSpecs & view remoteImageRepo
  let spec = DockerComposeServiceSpec (Just bin) (repo <> ":" <> bin <> ":" <> tag)
  return $
    DockerComposeService
      { name = bin,
        spec = spec
      }
