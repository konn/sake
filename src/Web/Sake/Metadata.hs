{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures, PatternGuards, TupleSections #-}
{-# LANGUAGE TypeFamilies                                        #-}
module Web.Sake.Metadata (Metadata, splitMetadata) where
import           Control.Arrow      (right)
import qualified Data.Aeson         as A
import qualified Data.List          as L
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml          as Y

type Metadata = A.Object

splitMetadata :: Text -> Either String (Metadata, Text)
splitMetadata src =
  let ls = dropWhile T.null $ T.lines src
  in case ls of
    (fense : rest)
      | T.all (== '-') fense
      , let fenseLen = T.length fense
      , fenseLen >= 3
      , (yaml, "---" : body) <- L.break (== fense) rest
        -> right (, T.unlines body) $ Y.decodeEither $ T.encodeUtf8 $ T.unlines yaml
    _ -> Right (mempty, src)
