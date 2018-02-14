{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies                               #-}
module Web.Sake.Template ( Context(..)
                         , Templatable(..)
                         , applyAsTemplate', applyTemplate
                         , loadTemplate, loadAndApplyTemplate, pathField
                         , metadataField, field, constField, bodyField
                         , defaultContext, titleField
                         ) where
import Web.Sake.Identifier
import Web.Sake.Item
import Web.Sake.Metadata

import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 (ToJSON, toJSON)
import           Data.Functor.Contravariant (Contravariant (..))
import qualified Data.HashMap.Strict        as HM
import           Data.Semigroup             (Semigroup (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           System.FilePath            (takeBaseName)

newtype Context a = Context { runContext :: forall m. MonadIO m => Item a -> m Metadata }

instance Contravariant Context where
  contramap f (Context g) = Context (g . fmap f)

instance Semigroup (Context a) where
  Context f <> Context g = Context $ \item ->
    (<>) <$> f item <*> g item

instance Monoid (Context a) where
  mempty = Context $ const $ return mempty
  mappend = (<>)

metadataField :: Context a
metadataField = Context $ return . itemMetadata

class Templatable tmpl where
  compileTemplate :: MonadIO m => Identifier -> Text -> m (Either String tmpl)
  applyToMetadata :: MonadIO m => tmpl -> Metadata -> m (Either String Text)

applyAsTemplate' :: forall tmpl proxy m. (Templatable tmpl, MonadIO m)
                 => proxy tmpl -> Context Text -> Item Text -> m (Item Text)
applyAsTemplate' _ ctx i@Item{..} = do
  compileTemplate @tmpl itemIdentifier itemBody >>= \case
    Left err   -> fail err
    Right tmpl -> applyTemplate tmpl ctx i

applyTemplate :: (Templatable tmpl, MonadIO m)
              => tmpl -> Context a -> Item a
              -> m (Item Text)
applyTemplate tmpl ctx i = do
  meta <- runContext ctx i
  applyToMetadata tmpl meta >>= \case
    Right a -> return $ i { itemBody = a }
    Left err -> fail err

loadTemplate :: forall tmpl m. (MonadIO m, Templatable tmpl) => FilePath -> m (Item tmpl)
loadTemplate fp = do
  i <- loadItem fp
  eith <- compileTemplate (itemIdentifier i) (itemBody i)
  case eith of
    Right t  -> return $ i { itemBody = t}
    Left err -> error err

loadAndApplyTemplate :: forall tmpl proxy a m.
                        (MonadIO m, Templatable tmpl)
                     => proxy tmpl     -- ^ Template
                     -> FilePath       -- ^ Template identifier
                     -> Context a      -- ^ Context
                     -> Item a         -- ^ Page
                     -> m (Item Text)  -- ^ Resulting item
loadAndApplyTemplate _ identifier context item = do
  tpl <- itemBody <$> loadTemplate @tmpl identifier
  applyTemplate tpl context item

field :: ToJSON a => String -> (forall m. MonadIO m => Item b -> m a) -> Context b
field k mk = Context $ \i -> do
  v <- mk i
  return $ HM.singleton (T.pack k) $ toJSON v

constField :: ToJSON a => String -> a -> Context b
constField k v =
  Context $ const $ return $
  HM.singleton (T.pack k) $ toJSON v

bodyField :: ToJSON a => String -> Context a
bodyField key = field key $ return . itemBody

pathField :: String -> Context a
pathField key = field key $ return . runIdentifier . itemIdentifier

defaultContext :: ToJSON a => Context a
defaultContext =
  mconcat [ bodyField "body", metadataField, pathField "path", titleField "title"]

titleField :: String -> Context a
titleField key = field key $ return . takeBaseName . runIdentifier . itemIdentifier
