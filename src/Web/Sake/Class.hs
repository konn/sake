{-# LANGUAGE FlexibleContexts, TypeApplications, TypeFamilies #-}
module Web.Sake.Class
       ( MonadSake(..)
       , needing
       , -- * Lifted functions WITHOUT dependency tracking
         readFileNoDep, readTextFileNoDep, readLazyTextFileNoDep
       , readBinaryFileNoDep, readLazyBinaryFileNoDep
       , copyFileNoDep
       , -- * Re-export(s)
         MonadIO(..)
       ) where
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.IO      as LT
import qualified Development.Shake      as Sh
import           System.Directory       (copyFile, createDirectoryIfMissing)
import           System.FilePath        (takeDirectory)

-- | Monad with @'Sh.Action'@ like dependency specification functionality.
--
--   Naming convention: functions with extra dependency tracking ends with @'@.
class (MonadIO m) => MonadSake m where
  need   :: [FilePath] -> m ()
  needed :: [FilePath] -> m ()

  createParentDirectoryFor :: FilePath -> m ()
  createParentDirectoryFor fp =
    liftIO $
    createDirectoryIfMissing True $ takeDirectory fp

  -- | @'writeFile' fp src@ writes the content of @src@ to @fp@,
  --   creating parent directories if missing.
  writeStringFile :: FilePath -> String -> m ()
  writeStringFile fp src = liftIO $ do
    createParentDirectoryFor fp
    writeFile fp src

  writeTextFile :: FilePath -> T.Text -> m ()
  writeTextFile fp src = liftIO $ do
    createParentDirectoryFor fp
    T.writeFile fp src

  writeLazyTextFile :: FilePath -> LT.Text -> m ()
  writeLazyTextFile fp src = liftIO $ do
    createParentDirectoryFor fp
    LT.writeFile fp src

  -- | @'copyFile' from to@  copies file @from@ to @to@,
  --   explicitly adding @from@ to dependency
  copyFile'  :: FilePath -> FilePath -> m ()
  copyFile' from to = do
    createParentDirectoryFor to
    need [from]
    liftIO $ copyFile from to

  readFile' :: FilePath -> m String
  readFile' = needing (liftIO . readFile)

  readTextFile' :: FilePath -> m T.Text
  readTextFile' = needing (liftIO . T.readFile)

  readLazyTextFile' :: FilePath -> m LT.Text
  readLazyTextFile' = needing (liftIO . LT.readFile)

  readBinaryFile' :: FilePath -> m BS.ByteString
  readBinaryFile' = needing (liftIO . BS.readFile)

  readLazyBinaryFile' :: FilePath -> m LBS.ByteString
  readLazyBinaryFile' = needing (liftIO . LBS.readFile)

  putNormal :: String -> m ()
  putLoud   :: String -> m ()
  putQuiet  :: String -> m ()

readFileNoDep :: MonadSake m => FilePath -> m String
readFileNoDep = liftIO . readFile

readTextFileNoDep :: MonadSake m => FilePath -> m T.Text
readTextFileNoDep = liftIO . T.readFile

readLazyTextFileNoDep :: MonadSake m => FilePath -> m LT.Text
readLazyTextFileNoDep = liftIO . LT.readFile

readBinaryFileNoDep :: MonadSake m => FilePath -> m BS.ByteString
readBinaryFileNoDep = liftIO . BS.readFile

readLazyBinaryFileNoDep :: MonadSake m => FilePath -> m LBS.ByteString
readLazyBinaryFileNoDep = liftIO . LBS.readFile

copyFileNoDep :: MonadSake m => FilePath -> FilePath -> m ()
copyFileNoDep from to = liftIO $ copyFile from to

needing :: MonadSake m => (FilePath -> m b) -> FilePath -> m b
needing f fp = need [fp] >> f fp

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

  writeStringFile = Sh.writeFile'
  copyFile'  = Sh.copyFile'
  readFile'  = Sh.readFile'

  putNormal = Sh.putNormal
  putLoud   = Sh.putLoud
  putQuiet  = Sh.putQuiet
