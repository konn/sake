{-# LANGUAGE TemplateHaskell #-}
module Web.Sake.Feed
       ( FeedConf(..), FeedAuthor(..)
       , renderRss, renderAtom
       ) where
import Web.Sake.Item
import Web.Sake.Template.Mustache

import           Control.Monad.IO.Class   (MonadIO)
import           Data.Aeson
import           Data.Char                (toLower)
import           Data.Hashable            (Hashable)
import qualified Data.HashMap.Strict      as HM
import           Data.Monoid              ((<>))
import           Data.Store               (Store)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           Text.Mustache            (Template)
import           Text.Mustache.Compile.TH (compileMustacheDir)

rssTemplate :: Template
rssTemplate = $(compileMustacheDir "rss" "data")

atomTemplate :: Template
atomTemplate = $(compileMustacheDir "atom" "data")

data FeedConf = FeedConf { feedTitle       :: Text
                         , feedDescription :: Text
                         , feedAuthor      :: FeedAuthor
                         , feedRoot        :: Text
                         }
                deriving ( Read, Show, Eq, Ord
                         , Generic, Hashable, Store )

data FeedAuthor = FeedAuthor { authorName  :: Text
                             , authorEmail :: Text
                             }
                deriving ( Read, Show, Eq, Ord
                         , Generic, Hashable, Store )

feedConfOptions :: Options
feedConfOptions =
  defaultOptions { fieldLabelModifier = map toLower . drop 4
                 }

feedAuthorOptions :: Options
feedAuthorOptions =
  defaultOptions { fieldLabelModifier = map toLower . drop 6
                 }

instance ToJSON FeedAuthor where
  toJSON = genericToJSON feedAuthorOptions

instance FromJSON FeedAuthor where
  parseJSON = genericParseJSON feedAuthorOptions

instance ToJSON FeedConf where
  toJSON = genericToJSON feedConfOptions

instance FromJSON FeedConf where
  parseJSON = genericParseJSON feedConfOptions

renderRss :: MonadIO m => FeedConf -> Context Text -> [Item Text] -> m Text
renderRss = renderFeed rssTemplate

renderAtom :: MonadIO m => FeedConf -> Context Text -> [Item Text] -> m Text
renderAtom = renderFeed atomTemplate

renderFeed :: (Templatable tmpl, MonadIO m) => tmpl -> FeedConf -> Context Text -> [Item Text] -> m Text
renderFeed tmpl conf itemCtx is = do
  let itemCtx' = mconcat [bodyField "description", itemCtx]
  items <- mapM (runContext itemCtx') is
  let Object dic = toJSON conf
      obj = HM.fromList [("items", toJSON items)] <> dic
  Right src <- applyToMetadata tmpl obj
  return src
