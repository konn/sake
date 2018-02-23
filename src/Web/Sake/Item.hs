{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, LambdaCase     #-}
{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, TypeApplications #-}
module Web.Sake.Item
       ( Item(..), loadItem, loadBinary, loadJSON, loadYaml
       , readPandoc, writePandoc, compilePandoc, loadMetadata
       ) where
import Web.Sake.Class
import Web.Sake.Identifier
import Web.Sake.Metadata

import qualified Data.Aeson              as A
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.Store              (Store)
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           GHC.Generics            (Generic)
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
            deriving (Read, Show, Eq, Functor, Generic, Hashable)

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
  case A.fromJSON  @MetaValue val of
    A.Success _ -> True
    _           -> False

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
loadMetadata path = itemMetadata <$> loadItem @MetadataOnly path
