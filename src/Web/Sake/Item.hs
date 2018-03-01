{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable, LambdaCase, NamedFieldPuns    #-}
{-# LANGUAGE NoMonomorphismRestriction, PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables             #-}
module Web.Sake.Item
       ( Item(..), loadItem, loadBinary, loadJSON, loadYaml
       , readPandoc, writePandoc, compilePandoc, loadMetadata
       , lookupMetadata, itemPath, setItemBody, itemDate'
       , itemPublishedDate, itemUpdatedDate
       ) where
import Web.Sake.Class
import Web.Sake.Identifier
import Web.Sake.Metadata

import qualified Data.Aeson              as A
import           Data.Foldable           (asum)
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.Store              (Store)
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Time               (ZonedTime, defaultTimeLocale,
                                          parseTimeM, utcToLocalZonedTime)
import           GHC.Generics            (Generic)
import           System.Directory        (getModificationTime)
import           System.FilePath         (takeExtension)
import           Text.Pandoc             (MetaValue (..), Pandoc (..), PandocIO,
                                          Reader (..), ReaderOptions,
                                          WriterOptions, getReader,
                                          readerExtensions, runIO, unMeta,
                                          writeHtml5String, writerExtensions)
import           Text.Pandoc.Shared      (stringify)

data Item a = Item { itemBody       :: a
                   , itemMetadata   :: Metadata
                   , itemIdentifier :: Identifier
                   }
            deriving (Read, Show, Eq, Functor,
                      Generic, Hashable, Traversable, Foldable
                     )

-- | Loads item, decomposing metadata if necessary.
loadItem :: (Readable a, MonadSake m) => FilePath -> m (Item a)
loadItem fp = do
  eith <- decompMetadata <$> readFromFile' fp
  case eith of
    Left err -> error err
    Right (meta, body) ->
      return $ Item body meta $ Identifier fp

loadBinary :: (Store a, MonadSake m) => FilePath -> m (Item a)
loadBinary = loadUnwrapped runBinary

loadYaml :: (MonadSake m, A.FromJSON a) => FilePath -> m (Item a)
loadYaml = loadUnwrapped runYaml

loadJSON :: (MonadSake m, A.FromJSON a) => FilePath -> m (Item a)
loadJSON = loadUnwrapped runJSON

loadUnwrapped :: (Readable (f a), MonadSake m) => (f a -> a) -> FilePath -> m (Item a)
loadUnwrapped unwrap fp = do
  body <- readFromFile' fp
  return $ Item (unwrap body) mempty $ Identifier fp

extensionDict :: [(String, String)]
extensionDict =
  [("tex", "latex")
  ,("md", "markdown")
  ,("html", "html")
  ]

feedReader :: Reader m -> ReaderOptions -> Text -> m Pandoc
feedReader (TextReader r) opt       = r opt
feedReader (ByteStringReader r) opt = r opt . LT.encodeUtf8 . LT.fromStrict

isMetaValue :: A.Value -> Bool
isMetaValue val =
  case A.fromJSON val of
    A.Success (_ :: MetaValue) -> True
    _                          -> False

-- | Compile the given @'Item'@ into @'Pandoc'@, taking care of metadatas.
--   Format is deteremined by extension.
readPandoc :: MonadSake m => ReaderOptions -> Item Text -> m (Item Pandoc)
readPandoc opts i@Item{..} = do
  let ext    = takeExtension $ runIdentifier itemIdentifier
      format = fromMaybe ext $ lookup (drop 1 ext) extensionDict
  runPandoc $
    case getReader format of
      Left err -> error err
      Right (rd, exts) -> do
        Pandoc metaPan body <- feedReader rd
                               opts { readerExtensions = readerExtensions opts <> exts
                                    }
                               itemBody
        let A.Success metaPanDef = A.fromJSON $ A.toJSON $ HM.filter isMetaValue itemMetadata
            metaPan' = metaPan <> metaPanDef
            A.Object panMeta = A.toJSON $ fromMetaValue <$> unMeta metaPan
            metadata' = itemMetadata <> panMeta
        return $ i { itemBody = Pandoc metaPan' body
                   , itemMetadata = metadata'
                   }

fromMetaValue :: MetaValue -> A.Value
fromMetaValue (MetaMap dic)     = A.toJSON $ fmap fromMetaValue dic
fromMetaValue (MetaList vs)     = A.toJSON $ fmap fromMetaValue vs
fromMetaValue (MetaBool b)      = A.toJSON b
fromMetaValue (MetaString s)    = A.toJSON s
fromMetaValue (MetaInlines inl) = A.toJSON $ stringify inl
fromMetaValue (MetaBlocks bls)  = A.toJSON $ stringify bls

runPandoc :: MonadSake m => PandocIO b -> m b
runPandoc act = liftIO (runIO act) >>= \case
    Left err  -> error $ show err
    Right pan -> return pan

-- | Write HTML by Pandoc.
writePandoc :: MonadSake m => WriterOptions -> Item Pandoc -> m (Item Text)
writePandoc opts i@Item{..} =
  runPandoc $ do
      src <- writeHtml5String
               opts { writerExtensions = writerExtensions opts
                    }
               itemBody
      return $ i { itemBody = src }

compilePandoc :: MonadSake m => ReaderOptions -> WriterOptions -> Item Text -> m (Item Text)
compilePandoc rOpt wOpt i =
  readPandoc rOpt i >>= writePandoc wOpt

loadMetadata :: MonadSake m => FilePath -> m Metadata
loadMetadata path = (itemMetadata :: Item MetadataOnly -> Metadata) <$> loadItem path


lookupMetadata :: A.FromJSON b => Text -> Item a -> Maybe b
lookupMetadata key Item{itemMetadata} =
  maybeResult . A.fromJSON =<< HM.lookup key itemMetadata

maybeResult :: A.Result a -> Maybe a
maybeResult (A.Success a) = Just a
maybeResult _             = Nothing

setItemBody :: a1 -> Item a2 -> Item a1
setItemBody bdy i = i { itemBody = bdy }

itemPath :: Item a -> FilePath
itemPath = runIdentifier . itemIdentifier

itemPublishedDate :: MonadIO m => Item a -> m ZonedTime
itemPublishedDate = itemDate' ["published", "date"] []

itemUpdatedDate :: MonadIO m => Item a -> m ZonedTime
itemUpdatedDate = itemDate' ["updated", "date"] []

itemDate' :: MonadIO m => [Text] -> [String] -> Item a -> m ZonedTime
itemDate' fields fmt0 item =
  let mdate = asum [readT =<< lookupMetadata f item | f <- fields ]
  in case mdate of
      Just date -> return date
      Nothing ->
        liftIO $
        utcToLocalZonedTime =<< getModificationTime (itemPath item)
  where
    readT i = asum [ parseTimeM True defaultTimeLocale f i | f <- fmts ]
    fmts =
      [ "%Y/%m/%d %X %Z"
      , "%a, %d %b %Y %H:%M:%S %Z"
      , "%Y-%m-%dT%H:%M:%S%Z"
      , "%Y-%m-%d %H:%M:%S%Z"
      , "%Y-%m-%d"
      , "%B %e, %Y %l:%M %p"
      , "%B %e, %Y"
      , "%b %d, %Y"
      ] ++ fmt0
