{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms, RankNTypes                    #-}
module Web.Sake.Route
       ( Route, idRoute, constRoute, withFilePath, setExtension
       ) where
import Web.Sake.Class

import Control.Monad              ((>=>))
import Data.Monoid                (Monoid (..))
import Data.Semigroup             (Semigroup (..))
import Development.Shake.FilePath

-- | Routing function.
--
--   Note that @'Data.String.IsString'@ instance is /intendedly/ unprovided,
--   because it beahaves counterintuitively together with @'Monoid'@ instances.
--   Use @'constRoute'@ for that purpose.
newtype Route =
  Route (forall m. MonadIO m => FilePath -> m FilePath )

-- | Routing with monadic composition forms @'Monoid'@.
--
--   __N.B.__ composition is in /reversed/ order: @f '<>' g@ applies @f@ first and then @g@.
instance Semigroup Route where
  Route f <> Route g = Route $ f >=> g

-- | Routing with monadic composition forms @'Monoid'@.
--
--   __N.B.__ composition is in /reversed/ order: @f '<>' g@ applies @f@ first and then @g@.
instance Monoid Route where
  mappend = (<>)
  mempty  = Route return

idRoute :: Route
idRoute = Route return

constRoute :: FilePath -> Route
constRoute fp = Route $ const $ return fp

withFilePath :: (FilePath -> FilePath) -> Route
withFilePath m = Route $ return . m

setExtension :: String -> Route
setExtension ext = withFilePath $ (-<.> ext)
