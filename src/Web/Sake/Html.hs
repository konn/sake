{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, TypeFamilies         #-}
-- | General HTML manipulation functions, similar to Hakyll
module Web.Sake.Html
       (mapTags, concatMapTags
       , withTags, mapTagTree
       , shiftHeadersBy, demoteHeaders
       , getUrls, withUrls
       , -- * Re-exports
         escapeHTML
       ) where
import Web.Sake.Utils

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.String            (fromString)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import           Text.HTML.TagSoup      (Tag (..), escapeHTML, parseTags,
                                         renderTags)
import           Text.HTML.TagSoup.Tree (TagTree, parseTree, renderTree,
                                         transformTree)
import           Text.StringLike        (StringLike, cons, uncons)

mapTags :: StringLike str
        => (Tag str -> Tag str)
        -> str -> str
mapTags trans = renderTags . map trans . parseTags
{-# SPECIALISE mapTags :: (Tag T.Text -> Tag T.Text) -> T.Text -> T.Text #-}
{-# SPECIALISE mapTags :: (Tag LT.Text -> Tag LT.Text) -> LT.Text -> LT.Text #-}
{-# SPECIALISE mapTags :: (Tag String -> Tag String) -> String -> String #-}
{-# SPECIALISE
    mapTags :: (Tag BS.ByteString -> Tag BS.ByteString)
            -> BS.ByteString -> BS.ByteString
 #-}
{-# SPECIALISE
    mapTags :: (Tag LBS.ByteString -> Tag LBS.ByteString)
            -> LBS.ByteString -> LBS.ByteString
 #-}
{-# INLINE mapTags #-}

withTags :: StringLike str
         => (Tag str -> Tag str)
         -> str -> str
withTags = mapTags
{-# INLINE withTags #-}

concatMapTags :: (StringLike str)
              => (Tag str -> [Tag str])
              -> str -> str
concatMapTags trans = renderTags . concatMap trans . parseTags
{-# SPECIALISE
    concatMapTags :: (Tag T.Text -> [Tag T.Text]) -> T.Text -> T.Text
 #-}
{-# SPECIALISE
    concatMapTags :: (Tag LT.Text -> [Tag LT.Text]) -> LT.Text -> LT.Text
 #-}
{-# SPECIALISE
    concatMapTags :: (Tag String -> [Tag String]) -> String -> String
 #-}
{-# SPECIALISE
    concatMapTags :: (Tag BS.ByteString -> [Tag BS.ByteString])
                  -> BS.ByteString -> BS.ByteString
 #-}
{-# SPECIALISE
    concatMapTags :: (Tag LBS.ByteString -> [Tag LBS.ByteString])
                  -> LBS.ByteString -> LBS.ByteString
 #-}
{-# INLINE concatMapTags #-}

mapTagTree :: (StringLike str)
           => (TagTree str -> [TagTree str])
           -> str -> str
mapTagTree trans = renderTree . transformTree trans . parseTree
{-# SPECIALISE
    mapTagTree :: (TagTree T.Text -> [TagTree T.Text]) -> T.Text -> T.Text
 #-}
{-# SPECIALISE
    mapTagTree :: (TagTree LT.Text -> [TagTree LT.Text]) -> LT.Text -> LT.Text
 #-}
{-# SPECIALISE
    mapTagTree :: (TagTree String -> [TagTree String]) -> String -> String
 #-}
{-# SPECIALISE
    mapTagTree :: (TagTree BS.ByteString -> [TagTree BS.ByteString])
                  -> BS.ByteString -> BS.ByteString
 #-}
{-# SPECIALISE
    mapTagTree :: (TagTree LBS.ByteString -> [TagTree LBS.ByteString])
                  -> LBS.ByteString -> LBS.ByteString
 #-}
{-# INLINE mapTagTree #-}

shiftHeadersBy :: (StringLike str) => Int -> str -> str
shiftHeadersBy offset = withTags $ \case
  TagOpen t atts
    | Just lvl <- headerLevel t -> TagOpen (getShifted lvl) atts
  TagClose t
    | Just lvl <- headerLevel t -> TagClose (getShifted lvl)
  t -> t
  where
    getShifted l = cons 'h' $ fromString (show $ max 1 $ min 6 $ l + offset)
    headerLevel str = do
      ('h', lvl) <- uncons str
      readM lvl
{-# SPECIALISE shiftHeadersBy :: Int -> T.Text -> T.Text #-}
{-# SPECIALISE shiftHeadersBy :: Int -> LT.Text -> LT.Text #-}
{-# SPECIALISE shiftHeadersBy :: Int -> String -> String #-}
{-# SPECIALISE shiftHeadersBy :: Int -> BS.ByteString -> BS.ByteString #-}
{-# SPECIALISE shiftHeadersBy :: Int -> LBS.ByteString -> LBS.ByteString #-}

demoteHeaders :: StringLike str => str -> str
demoteHeaders = shiftHeadersBy 1
{-# INLINE demoteHeaders #-}
{-# SPECIALISE demoteHeaders :: T.Text -> T.Text #-}
{-# SPECIALISE demoteHeaders :: LT.Text -> LT.Text #-}
{-# SPECIALISE demoteHeaders :: String -> String #-}
{-# SPECIALISE demoteHeaders :: BS.ByteString -> BS.ByteString #-}
{-# SPECIALISE demoteHeaders :: LBS.ByteString -> LBS.ByteString #-}

linkAtts :: (StringLike str) => [str]
linkAtts = ["href", "src", "data", "poster"]

getUrls :: (StringLike str) => str -> [str]
getUrls = concatMap qry . parseTags
  where
    qry (TagOpen _ atts) =
           [ url
           | (key, url) <- atts
           , key `elem` linkAtts
           ]
    qry _                = []
{-# INLINE getUrls #-}
{-# SPECIALISE getUrls :: T.Text -> [T.Text] #-}
{-# SPECIALISE getUrls :: LT.Text -> [LT.Text] #-}
{-# SPECIALISE getUrls :: String -> [String] #-}
{-# SPECIALISE getUrls :: BS.ByteString -> [BS.ByteString] #-}
{-# SPECIALISE getUrls :: LBS.ByteString -> [LBS.ByteString] #-}

withUrls :: (StringLike str) => (str -> str) -> str -> str
withUrls f = withTags $ \case
  TagOpen a atts -> TagOpen a $ map change atts
  i -> i
  where
    change (k, v)
      | k `elem` linkAtts = (k, f v)
      | otherwise = (k, v)

{-# INLINE withUrls #-}
{-# SPECIALISE withUrls :: (T.Text -> T.Text) -> T.Text -> T.Text #-}
{-# SPECIALISE withUrls :: (LT.Text -> LT.Text) -> LT.Text -> LT.Text #-}
{-# SPECIALISE withUrls :: (String -> String) -> String -> String #-}
{-# SPECIALISE withUrls :: (BS.ByteString -> BS.ByteString) -> BS.ByteString -> BS.ByteString #-}
{-# SPECIALISE withUrls :: (LBS.ByteString -> LBS.ByteString) -> LBS.ByteString -> LBS.ByteString #-}
