module Klon.Command.Query where

import Options.Applicative
import Klon.Cloud.Resources.AWS.ECR
import Klon.Prelude
import Network.AWS.ECR (ImageDetail(..), idImageTags)
import RIO

data QueryCmd = RemoteImageInfo

queryImages :: KlonM ()
queryImages = do
  mbImg <- anyServiceImageForCurrentCommit
  reportStatus mbImg
  where
    reportStatus mbImg = liftIO $
      case mbImg of 
        Just img -> print $ "Found image for current commit: " <> (head $ img ^. idImageTags)
        Nothing -> print "Image not currently in repo for current commit"

querySubCmdParser :: Parser QueryCmd 
querySubCmdParser =
  subparser
    ( command "images" (info (pure RemoteImageInfo) (progDesc "Current ECR Images")))