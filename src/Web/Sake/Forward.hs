{-# LANGUAGE DerivingStrategies, GADTs, GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE LambdaCase, PatternSynonyms, RankNTypes, RecordWildCards #-}
module Web.Sake.Forward (Sake, runSake, SakeAction, Key, create, match) where
import Web.Sake.Class
import Web.Sake.Conf
import Web.Sake.Identifier
import Web.Sake.Item
import Web.Sake.Route

import Control.Monad.Fail
import Control.Monad.Operational
import Control.Monad.Reader
import Development.Shake         (Action, FilePattern, Rules)

data SakeActionEnv = SAEnv { target :: FilePath
                           , conf   :: SakeConf
                           }

newtype SakeAction a = SakeAction { unSakeAction :: ReaderT SakeActionEnv Action a }
                     deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)

newtype Key a = Key { unKey :: Identifier }
              deriving (Show, Eq, Ord)

instance MonadSake SakeAction

instance MonadAction SakeAction where
  liftAction = SakeAction . lift

data Sake' a where
  Match  :: FilePattern -> Maybe Route -> SakeAction (Item a) -> Sake' (Key a)
  Create :: [FilePath]  -> SakeAction (Item a) -> Sake' a

match :: FilePattern -> Maybe Route -> SakeAction (Item a) -> Sake (Key a)
match pat mrou = Sake . singleton . Match pat mrou

create :: [FilePath] -> SakeAction (Item a) -> Sake a
create fps = Sake . singleton . Create fps

newtype Sake a = Sake { unSake :: Program Sake' a }
               deriving newtype (Functor, Monad, Applicative)

runSake :: SakeConf -> Sake a -> Rules a
runSake SakeConf{..} = interpretWithMonad go . unSake
  where
    go (Match pat mrou act) = do
      undefined
    go _  = undefined
