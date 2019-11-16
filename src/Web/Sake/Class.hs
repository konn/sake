{-# LANGUAGE ConstraintKinds, DefaultSignatures, FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction             #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, UndecidableInstances               #-}
module Web.Sake.Class
       ( MonadSake(..), MonadAction(..)
       , Writable(..), Readable(..)
       , needing
       , -- * Specialised functions
         writeStringFile, writeTextFile, writeLazyTextFile
       , writeBinaryFile, writeYamlFile, writeJSONFile
       , readStringFile', readTextFile', readLazyTextFile'
       , readFromBinaryFile', readFromJSONFile', readFromYamlFile'
       , -- * Lifted functions WITHOUT dependency tracking
         readFileNoDep, readTextFileNoDep, readLazyTextFileNoDep
       , readFromBinaryFileNoDep, readBinaryFileNoDep, readFromFileNoDep
       , copyFileNoDep, defaultStoreWriteTo_, defaultYamlWriteTo_
       , defaultJSONWriteTo_
       , defaultStoreReadFrom_, defaultJSONReadFrom_, defaultYamlReadFrom_
       , -- * Useful wrrapers
         Binary(..), Shown(..), Yaml(..), JSON(..)
       , CopyFile(..), TempFile(..), MetadataOnly(..)
       , -- * Re-export(s)
         MonadIO(..)
       ) where
import Web.Sake.Metadata
import Web.Sake.Utils

import           Control.Arrow                 (second)
import           Control.Exception             (throwIO)
import           Control.Monad                 ((<=<))
import           Control.Monad.Fail
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson                    (FromJSON, ToJSON)
import qualified Data.Aeson                    as Ae
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Store                    (Store)
import qualified Data.Store                    as Store
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.IO             as LT
import qualified Data.Yaml                     as Y
import qualified Development.Shake             as Sh
import           GHC.Generics                  (Generic)
import           System.Directory              (copyFile,
                                                createDirectoryIfMissing,
                                                renameFile)
import           System.FilePath               (takeDirectory)
import           Text.Blaze.Html               (Html)
import qualified Text.Blaze.Html.Renderer.Text as Html

-- | Monad with @'Sh.Action'@ like dependency specification functionality.
--
--   Naming convention: functions with extra dependency tracking ends with @'@.
class (MonadIO m, MonadFail m) => MonadSake m where
  need   :: [FilePath] -> m ()
  needed :: [FilePath] -> m ()

  default need :: MonadAction m => [FilePath] -> m ()
  need = liftAction . need

  default needed :: MonadAction m => [FilePath] -> m ()
  needed = liftAction . needed

  createParentDirectoryFor :: FilePath -> m ()
  createParentDirectoryFor fp =
    liftIO $
    createDirectoryIfMissing True $ takeDirectory fp

  -- | @'writeToFile' fp src@ writes the content of @src@ to @fp@,
  --   creating parent directories if missing.
  writeToFile :: Writable a => FilePath -> a -> m ()
  writeToFile fp src = liftIO $ do
    createParentDirectoryFor fp
    writeTo_ fp src

  -- | @'copyFile' from to@  copies file @from@ to @to@,
  --   explicitly adding @from@ to dependency
  copyFile'  :: FilePath -> FilePath -> m ()
  copyFile' from to = do
    createParentDirectoryFor to
    need [from]
    liftIO $ copyFile from to

  readFromFile' :: Readable a => FilePath -> m a
  readFromFile' = needing (liftIO . readFrom_)


  putNormal :: String -> m ()
  putLoud   :: String -> m ()
  putQuiet  :: String -> m ()

  default putNormal :: MonadAction m => String -> m ()
  putNormal = liftAction . putNormal

  default putLoud :: MonadAction m => String -> m ()
  putLoud = liftAction . putLoud

  default putQuiet :: MonadAction m => String -> m ()
  putQuiet = liftAction . putQuiet

class MonadSake m => MonadAction m where
  liftAction :: Sh.Action a -> m a

instance MonadAction Sh.Action where
  liftAction = id

writeStringFile :: (MonadSake m) => FilePath -> String -> m ()
writeStringFile = writeToFile


writeTextFile :: (MonadSake m) => FilePath -> T.Text -> m ()
writeTextFile = writeToFile


writeLazyTextFile :: (MonadSake m) => FilePath -> LT.Text -> m ()
writeLazyTextFile = writeToFile


readStringFile' :: (MonadSake m) => FilePath -> m String
readStringFile' = readFromFile'


readTextFile' :: (MonadSake m) => FilePath -> m T.Text
readTextFile' = readFromFile'


readLazyTextFile' :: (MonadSake m) => FilePath -> m LT.Text
readLazyTextFile' = readFromFile'


readFromBinaryFile' :: (MonadSake m, Store a) => FilePath -> m a
readFromBinaryFile' = fmap runBinary . readFromFile'


readFromYamlFile' :: (MonadSake m, FromJSON a) => FilePath -> m a
readFromYamlFile' = fmap runYaml . readFromFile'


readFromJSONFile' :: (MonadSake m, FromJSON a) => FilePath -> m a
readFromJSONFile' = fmap runJSON . readFromFile'


readFileNoDep :: MonadSake m => FilePath -> m String
readFileNoDep = liftIO . readFrom_

readTextFileNoDep :: MonadSake m => FilePath -> m T.Text
readTextFileNoDep = liftIO . readFrom_

readLazyTextFileNoDep :: MonadSake m => FilePath -> m LT.Text
readLazyTextFileNoDep = liftIO . readFrom_

readFromBinaryFileNoDep :: (MonadSake m, Store a) => FilePath -> m a
readFromBinaryFileNoDep = liftIO . fmap runBinary . readFrom_


readFromFileNoDep :: (MonadSake m, Readable a) => FilePath -> m a
readFromFileNoDep = liftIO . readFrom_


writeBinaryFile :: (Store a, MonadSake m) => FilePath -> a -> m ()
writeBinaryFile fp = writeToFile fp . Binary


writeYamlFile :: (MonadSake m, ToJSON a) => FilePath -> a -> m ()
writeYamlFile fp = writeToFile fp . Yaml


writeJSONFile :: (MonadSake m, ToJSON a) => FilePath -> a -> m ()
writeJSONFile fp = writeToFile fp . JSON


copyFileNoDep :: MonadSake m => FilePath -> FilePath -> m ()
copyFileNoDep from to = liftIO $ do
  createParentDirectoryFor to
  copyFile from to

needing :: MonadSake m => (FilePath -> m b) -> FilePath -> m b
needing f fp = need [fp] >> f fp

readBinaryFileNoDep :: MonadIO m => FilePath -> m BS.ByteString
readBinaryFileNoDep = liftIO . BS.readFile

-- | Just for testing; ignors @'need'@ and @'needed'@.
instance MonadSake IO where
  need _ = return ()
  needed _ = return ()

  putNormal = putStrLn
  putLoud   = putStrLn
  putQuiet  = putStrLn

instance MonadSake Sh.Action where
  need = Sh.need
  needed = Sh.needed

  copyFile'  = Sh.copyFile'

  putNormal = Sh.putNormal
  putLoud   = Sh.putLoud
  putQuiet  = Sh.putQuiet

class Writable a where
  writeTo_ :: MonadSake m => FilePath -> a -> m ()

instance Writable BS.ByteString where
  writeTo_ fp = liftIO .  BS.writeFile fp


instance Writable LBS.ByteString where
  writeTo_ fp = liftIO . LBS.writeFile fp


instance Writable T.Text where
  writeTo_ fp = liftIO . T.writeFile fp


instance Writable LT.Text where
  writeTo_ fp = liftIO . LT.writeFile fp

instance Writable String where
  writeTo_ fp = liftIO . writeFile fp


instance Writable Html where
  writeTo_ fp = liftIO . writeTo_ fp . Html.renderHtml


newtype Binary a = Binary { runBinary :: a }
                 deriving (Read, Show, Eq, Ord, Generic)

instance Store a => Writable (Binary a) where
  writeTo_ fp = liftIO . writeTo_ fp . Store.encode . runBinary


newtype Shown a = Shown { runShown :: a }
                deriving (Read,Show,  Eq, Ord, Generic)

defaultStoreWriteTo_ :: (MonadSake m, Store a) => FilePath -> a -> m ()
defaultStoreWriteTo_ fp = writeTo_ fp . Binary

defaultYamlWriteTo_ :: (MonadSake m, ToJSON a) => FilePath -> a -> m ()
defaultYamlWriteTo_ fp = writeTo_ fp . Yaml

defaultJSONWriteTo_ :: (MonadSake m, ToJSON a) => FilePath -> a -> m ()
defaultJSONWriteTo_ fp = writeTo_ fp . JSON

instance Show a => Writable (Shown a) where
  writeTo_ fp = writeTo_ fp . show . runShown


newtype Yaml a = Yaml { runYaml :: a }
               deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON a => Writable (Yaml a) where
  writeTo_ fp = writeTo_ fp . Y.encode . runYaml


newtype JSON a = JSON { runJSON :: a }
               deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON a => Writable (JSON a) where
  writeTo_ fp = writeTo_ fp . Ae.encode . runJSON


class Readable a where
  readFrom_ :: FilePath -> IO a
  decompMetadata :: a -> Either String (Metadata, a)
  decompMetadata a = Right (mempty, a)

instance Readable String where
  readFrom_ = readFile

  decompMetadata = fmap (second T.unpack) . splitMetadata . T.pack


instance Readable T.Text where
  readFrom_ = T.readFile

  decompMetadata = splitMetadata


instance Readable LT.Text where
  readFrom_ = LT.readFile

  decompMetadata = fmap (second LT.fromStrict) . splitMetadata . LT.toStrict


instance Readable BS.ByteString where
  readFrom_ = BS.readFile


instance Readable LBS.ByteString where
  readFrom_ = LBS.readFile


instance Store a => Readable (Binary a) where
  readFrom_ = either throwIO (return . Binary) . Store.decode <=< readFrom_

instance Read a => Readable (Shown a) where
  readFrom_ = fmap Shown . (readM :: String -> IO a)  <=< readFrom_

instance FromJSON a => Readable (Yaml a) where
  readFrom_ = either (throwIO . userError) (return . Yaml) . Y.decodeEither <=< readFrom_

instance FromJSON a => Readable (JSON a) where
  readFrom_ = either (throwIO . userError) (return . JSON) . Ae.eitherDecode <=< readFrom_

defaultStoreReadFrom_ :: Store b => FilePath -> IO b
defaultStoreReadFrom_ = fmap runBinary . readFrom_

defaultYamlReadFrom_ :: FromJSON b => FilePath -> IO b
defaultYamlReadFrom_ = fmap runYaml . readFrom_

defaultJSONReadFrom_ :: FromJSON b => FilePath -> IO b
defaultJSONReadFrom_ = fmap runJSON . readFrom_

newtype CopyFile = CopyFile FilePath
              deriving (Read, Show, Eq, Ord)

instance Writable CopyFile where
  writeTo_ fp (CopyFile from) = copyFile' from fp

newtype TempFile = TempFile FilePath
              deriving (Read, Show, Eq, Ord)

instance Writable TempFile where
  writeTo_ fp (TempFile from) = liftIO $ do
    createDirectoryIfMissing True (takeDirectory fp)
    renameFile from fp

newtype MetadataOnly = MetadataOnly Metadata
                deriving (Read, Show, Eq, Generic)

instance Readable MetadataOnly where
  readFrom_ fp =
    either error (MetadataOnly . fst) . splitMetadata <$> readFrom_ fp

  decompMetadata i@(MetadataOnly m) = Right (m, i)


