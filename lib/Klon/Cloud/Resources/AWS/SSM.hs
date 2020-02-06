{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
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

class FromSSM a where
  fromSSM :: forall a. (FromJSON a, Generic a, Selectors (Rep a)) => (Text -> Text) -> AWS (Maybe a)
  default fromSSM :: forall a m. (Generic a, FromJSON a, Selectors (Rep a)) => (Text -> Text) -> AWS (Maybe a)
  fromSSM withModifyField = (gSetFromSSM @a) withModifyField

gSetFromSSM :: forall r. (FromJSON r, Selectors (Rep r)) => (Text -> Text) -> AWS (Maybe r)
gSetFromSSM modifyField = do
  let fNames =
        (selectors (Proxy @(Rep r)))
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

class Selectors rep where
  selectors :: Proxy rep -> [String]

instance Selectors f => Selectors (M1 D x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    [selName (undefined :: M1 S s (K1 R t) ())]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

instance Selectors U1 where
  selectors _ = []