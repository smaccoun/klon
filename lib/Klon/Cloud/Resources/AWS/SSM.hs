{-# LANGUAGE DeriveDataTypeable #-}

module Klon.Cloud.Resources.AWS.SSM where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Data
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text (..))
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding
import Data.Tuple.Sequence (sequenceT)
import GHC.Generics
import Lens.Micro
import Network.AWS
import Network.AWS.SSM.GetParameters (getParameters, grsParameters)
import Network.AWS.SSM.Types (pName, pValue)

getParams :: MonadAWS m => NonEmpty Text -> m [(Text, Text)]
getParams paramsToGet = do
  paramResp <- send $ getParameters paramsToGet
  let params = paramResp ^. grsParameters
      asMaybeList :: [Maybe (Text, Text)]
      asMaybeList = (\p -> sequenceT (p ^. pName, p ^. pValue)) <$> params
  return $ fromMaybe [] $ sequence asMaybeList

data PGConf
  = PGConf
      { pgPort :: Int,
        pgUser :: Text,
        pgDb   :: Text,
        pgPass :: Text,
        pgHost :: Text
      }
  deriving (Generic, FromJSON, Show, Data)

gSetFromSSM :: (FromJSON r, Data r) => Proxy r -> (Text -> Text) -> AWS (Maybe r)
gSetFromSSM proxy modifyField = do
  let fNames =
        (recordFieldNames proxy)
          & fmap (modifyField . T.pack)
          & NE.fromList
  paramList <- getParams fNames
  let decoded = Data.Aeson.decode $ encodeUtf8 $ asJSON_Rec paramList
  return decoded
  where
    asJSON_Rec paramList' =
      L.fromStrict $
        "{"
          <> (T.intercalate ", " $ jsonObjFields paramList')
          <> "}"
    jsonObjFields =
      fmap (\(k, v) -> k <> ": " <> v)

recordFieldNames :: forall r. Data r => Proxy r -> [String]
recordFieldNames _ =
  concat $
    dataTypeOf (undefined :: r) 
      & dataTypeConstrs
      & fmap constrFields
