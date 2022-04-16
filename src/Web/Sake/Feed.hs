{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Web.Sake.Feed
  ( FeedConf (..),
    FeedAuthor (..),
    renderRss,
    renderAtom,
  )
where

import Control.Monad ((<=<))
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toLower)
import Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Store (Store)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (ZonedTime, zonedTimeToUTC)
import Data.Time.Format
  ( defaultTimeLocale,
    formatTime,
    parseTimeM,
  )
import GHC.Generics (Generic)
import Text.Mustache (Template)
import Text.Mustache.Compile.TH (compileMustacheDir)
import Web.Sake.Class
import Web.Sake.Item
import Web.Sake.Template.Mustache

rssTemplate :: Template
rssTemplate = $(compileMustacheDir "rss" "data")

atomTemplate :: Template
atomTemplate = $(compileMustacheDir "atom" "data")

data FeedConf = FeedConf
  { feedTitle :: Text
  , feedDescription :: Text
  , feedAuthor :: FeedAuthor
  , feedRoot :: Text
  , feedUrl :: Text
  }
  deriving
    ( Read
    , Show
    , Eq
    , Ord
    , Generic
    , Hashable
    , Store
    )

instance Default FeedConf where
  def =
    FeedConf
      { feedTitle = ""
      , feedDescription = ""
      , feedAuthor = FeedAuthor "" ""
      , feedRoot = "http://example.com"
      , feedUrl = "/feed.xml"
      }

data FeedAuthor = FeedAuthor
  { authorName :: Text
  , authorEmail :: Text
  }
  deriving
    ( Read
    , Show
    , Eq
    , Ord
    , Generic
    , Hashable
    , Store
    )

feedConfOptions :: Options
feedConfOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop 4
    }

feedAuthorOptions :: Options
feedAuthorOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop 6
    }

instance ToJSON FeedAuthor where
  toJSON = genericToJSON feedAuthorOptions

instance FromJSON FeedAuthor where
  parseJSON = genericParseJSON feedAuthorOptions

instance ToJSON FeedConf where
  toJSON = genericToJSON feedConfOptions

instance FromJSON FeedConf where
  parseJSON = genericParseJSON feedConfOptions

renderRss :: MonadSake m => FeedConf -> Context Text -> [Item Text] -> m Text
renderRss = renderFeed rssTemplate

renderAtom :: MonadSake m => FeedConf -> Context Text -> [Item Text] -> m Text
renderAtom = renderFeed atomTemplate

renderFeed ::
  (Templatable tmpl, MonadSake m) =>
  tmpl ->
  FeedConf ->
  Context Text ->
  [Item Text] ->
  m Text
renderFeed tmpl conf itemCtx is = do
  let itemCtx' =
        mconcat
          [ itemCtx
          , field "updated" $ fmap formatForAtomDate . itemUpdatedDate
          , bodyField "description"
          ]
  items <- mapM (runContext itemCtx') is
  let Object dic = toJSON conf
      upd =
        L.maximumBy (comparing zonedTimeToUTC) $
          mapMaybe
            ( parseTimeM @Maybe @ZonedTime True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
                <=< maybeResult . fromJSON
                <=< KM.lookup "updated"
            )
            items
      obj =
        KM.fromList [("items", toJSON items), ("updated", toJSON upd)]
          <> KM.delete "dateformat" dic
  Right src <- applyToMetadata tmpl obj
  return src

maybeResult :: Result a -> Maybe a
maybeResult (Success a) = Just a
maybeResult _ = Nothing

formatForAtomDate :: ZonedTime -> Text
formatForAtomDate = insertColon . T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
  where
    insertColon txt =
      let (bh, ah) = T.splitAt (T.length txt - 2) txt
       in bh <> ":" <> ah
