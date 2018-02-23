{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}
module Web.Sake.Given
       (SakeConf(..), DeployMethod(..), clean,
        withSakeConf, siteContent, cached, copy,
        loadItem, loadBinary, loadYaml, loadJSON,
        module Web.Sake
       ) where
import           Web.Sake      hiding (loadBinary, loadItem, loadJSON, loadYaml,
                                writeFile')
import           Web.Sake.Conf
import qualified Web.Sake.Item as Item

import Data.Aeson      (FromJSON)
import Data.Default    (Default (..))
import Data.Reflection (Given (..), give)
import Data.Store      (Store)
import GHC.Generics    (Generic)
import System.Exit     (ExitCode)

withSakeConf :: SakeConf -> (Given SakeConf => a) -> a
withSakeConf cnf = give cnf

conf :: Given SakeConf => SakeConf
conf = given

copy :: Given SakeConf => FilePath -> Action (Item CopyFile)
copy fp =
  return $ Item (CopyFile (toSourcePath fp)) mempty $
           Identifier fp

siteContent :: (Writable a, Given SakeConf) => FilePattern -> (String -> Action (Item a)) -> Rules ()
siteContent fp act = do
  cached fp act
  (destinationDir conf </> fp) %> \ out -> do
    copyFile' (toCachePath out) out

cached :: (Writable a, Given SakeConf) => FilePattern -> (String -> Action (Item a)) -> Rules ()
cached cacheName act =
  (cacheDir conf </> cacheName) %> \ out -> do
    i <- act (dropCache out)
    writeToFile out $ itemBody i

dropDest, dropCache, toSourcePath, toCachePath :: Given SakeConf => FilePath -> FilePath
dropDest = makeRelative (destinationDir conf)
dropCache = makeRelative (cacheDir conf)
toSourcePath = replaceDir (destinationDir conf) (sourceDir conf)
toCachePath = replaceDir (destinationDir conf) (cacheDir conf)

replaceDir :: FilePath -> FilePath -> FilePath -> FilePath
replaceDir from to pth = to </> makeRelative from pth

loadItem :: (MonadSake m, Readable a, Given SakeConf) => FilePath -> m (Item a)
loadItem = Item.loadItem . toSourcePath

loadBinary :: (Given SakeConf, Store a, MonadSake m) => FilePath -> m (Item a)
loadBinary = Item.loadBinary . toSourcePath

loadYaml :: (Given SakeConf, MonadSake m, FromJSON a) => FilePath -> m (Item a)
loadYaml = Item.loadYaml . toSourcePath

loadJSON :: (Given SakeConf, MonadSake m, FromJSON a) => FilePath -> m (Item a)
loadJSON = Item.loadJSON . toSourcePath

clean :: Given SakeConf => Rules ()
clean = phony "clean" $ do
  removeFilesAfter (destinationDir conf) ["//*"]
  removeFilesAfter (cacheDir conf)       ["//*"]
