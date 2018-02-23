module Web.Sake.Conf (SakeConf(..), DeployMethod(..)) where
import Data.Default (Default (..))
import GHC.Generics (Generic)
import System.Exit  (ExitCode (..))

data SakeConf = SakeConf { destinationDir :: FilePath
                         , cacheDir       :: FilePath
                         , sourceDir      :: FilePath
                         , ignoreFile     :: FilePath -> Bool
                         , deployMethod   :: DeployMethod
                         , previewHost    :: String
                         , previewPort    :: Int
                         }
              deriving (Generic)

instance Default SakeConf where
  def = SakeConf { destinationDir = "_site"
                 , cacheDir = "_cache"
                 , sourceDir = "src-site"
                 , ignoreFile = const False
                 , deployMethod = DeployCommand "echo \"No deployment method provided!\""
                 , previewHost = "localhost"
                 , previewPort = 8000
                 }

data DeployMethod = DeployCommand { deployCommand :: String }
                  | DeployAction { deployAction :: SakeConf -> IO ExitCode }
                  deriving (Generic)
