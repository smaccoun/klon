module Klon.Cloud.Resources.AWS.SSM where

import Data.Aeson
import Data.Data
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text (Text (..))
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding
import Lens.Micro
import Network.AWS
import Network.AWS.SSM.GetParameters (getParameters, grsParameters)
import Network.AWS.SSM.Types (pName, pValue)
import Control.Monad.IO.Class (liftIO)
import Data.Tuple.Sequence (sequenceT)
import Data.Maybe (fromMaybe)
import Data.Proxy

getParams :: MonadAWS m => NonEmpty Text -> m [(Text, Text)]
getParams paramsToGet = do
  paramResp <- send $ getParameters paramsToGet
  let params = paramResp ^. grsParameters
      asMaybeList :: [Maybe (Text, Text)] 
      asMaybeList = (\p -> sequenceT (p ^. pName, p ^. pValue)) <$> params
  return $ fromMaybe [] $ sequence asMaybeList

data PGConf = PGConf {pgPort :: Int, pgUser :: Text} deriving (Generic, FromJSON, Show, Typeable, Data)

gSetFromSSM :: (FromJSON r, Data r) => Proxy r -> AWS r
gSetFromSSM proxy = do
  let fNames = 
        (recordFieldNames proxy)
          & fmap T.pack
          & NE.fromList
  paramList <- getParams fNames
  let Just r = Data.Aeson.decode $ encodeUtf8 $ asJSON_Rec paramList
  return $ r
  where
    asJSON_Rec paramList' = L.fromStrict $
      "{"
        <> (T.intercalate ", " $ jsonObjFields paramList')
        <> "}"
    jsonObjFields =
      fmap (\(k, v) -> k <> ": " <> v)

recordFieldNames :: Data r => Proxy r -> [String]
recordFieldNames proxy = concat $
  dataTypeOf proxy 
  & dataTypeConstrs
  & fmap constrFields
