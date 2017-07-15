{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Twitcher where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import qualified Data.ByteString as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Network.Wreq
import Control.Exception
import Control.Lens (view, (.~), (&))

data Stream = Stream { id :: Int
                     , game :: String
                     , viewers :: Int
                     , videoHeight :: Int
                     , averageFps :: Float
                     , delay :: Int
                     , channel :: Channel
                     , createdAt :: UTCTime
                     } deriving (Show)

instance A.FromJSON Stream where
  parseJSON (AT.Object o) =
    Stream <$>
    (o A..: "_id") <*>
    (o A..: "game") <*>
    (o A..: "viewers") <*>
    (o A..: "video_height") <*>
    (o A..: "average_fps") <*>
    (o A..: "delay") <*>
    (o A..: "channel") <*>
    (o A..: "created_at")
  parseJSON invalid = AT.typeMismatch "Stream" invalid
              
data Channel = Channel { id :: Int
                       , broadcasterLanguage :: String
                       , displayName :: Text
                       , followers :: Int
                       , game :: Text
                       , language :: Text
                       , logo :: Text
                       , mature :: Bool
                       , name :: Text
                       , partner :: Bool
                       , profileBanner :: Maybe Text
                       , profileBannerBackgroundColor :: Maybe Text
                       , status :: Text
                       , createdAt :: UTCTime
                       , updatedAt :: UTCTime
                       , url :: Text
                       , videoBanner :: Maybe Text
                       , views :: Int
                       } deriving (Show)
               
instance A.FromJSON Channel where
  parseJSON (AT.Object o) =
    Channel <$>
    (o A..: "_id") <*>
    (o A..: "broadcaster_language") <*>
    (o A..: "display_name") <*>
    (o A..: "followers") <*>
    (o A..: "game") <*>
    (o A..: "language") <*>
    (o A..: "logo") <*>
    (o A..: "mature") <*>
    (o A..: "name") <*>
    (o A..: "partner") <*>
    (o A..: "profile_banner") <*>
    (o A..: "profile_banner_background_color") <*>
    (o A..: "status") <*>
    (o A..: "created_at") <*>
    (o A..: "updated_at") <*>
    (o A..: "url") <*>
    (o A..: "video_banner") <*>
    (o A..: "views")
  parseJSON invalid = AT.typeMismatch "Channel" invalid

data FollowedResponse = FollowedResponse { total :: Int
                                         , streams :: [Stream]
                                         } deriving (Show)

instance A.FromJSON FollowedResponse where
  parseJSON (AT.Object o) =
    FollowedResponse <$>
    (o A..: "_total") <*>
    (o A..: "streams")
  parseJSON invalid = AT.typeMismatch "FollowedResponse" invalid

getFollowed :: Text -> Text -> IO (Either SomeException FollowedResponse)
getFollowed clientid authorization = do
  let opts = defaults &
             param "limit" .~ ["100"] &
             header "Client-ID" .~ [encodeUtf8 clientid] &
             header "Authorization" .~ [B.concat ["OAuth ", encodeUtf8 authorization]]
  req <- getWith opts "https://api.twitch.tv/kraken/streams/followed"
  return $ fmap (view responseBody) (asJSON req)
