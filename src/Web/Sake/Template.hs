{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeFamilies              #-}
module Web.Sake.Template ( Context(..)
                         , Templatable(..)
                         , applyAsTemplate', applyTemplate
                         , loadTemplate, loadAndApplyTemplate, pathField
                         , metadataField, field, constField, bodyField
                         , defaultContext, titleField, listField
                         , objectContext, applyTemplateList, applyJoinTemplateList
                         , dynField, field_
                         ) where
import Web.Sake.Class
import Web.Sake.Identifier
import Web.Sake.Item
import Web.Sake.Metadata

import           Control.Monad              ((<=<))
import           Data.Aeson                 (ToJSON, Value (Object), encode,
                                             toJSON)
import           Data.Functor.Contravariant (Contravariant (..))
import qualified Data.HashMap.Strict        as HM
import           Data.Semigroup             (Semigroup (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT
import           Language.Haskell.TH        (ExpQ, Name, listE, litE, nameBase,
                                             stringL, varE)
import           System.FilePath            (takeBaseName)

newtype Context a = Context { runContext :: forall m. MonadSake m => Item a -> m Metadata }

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
  compileTemplate :: MonadSake m => Identifier -> Text -> m (Either String tmpl)
  applyToMetadata :: MonadSake m => tmpl -> Metadata -> m (Either String Text)

applyAsTemplate' :: forall tmpl proxy m. (Templatable tmpl, MonadSake m)
                 => proxy tmpl -> Context Text -> Item Text -> m (Item Text)
applyAsTemplate' _ ctx i@Item{..} =
  compileTemplate @tmpl itemIdentifier itemBody >>= \case
    Left err   -> fail err
    Right tmpl -> applyTemplate tmpl ctx i

applyTemplate :: (Templatable tmpl, MonadSake m)
              => tmpl -> Context a -> Item a
              -> m (Item Text)
applyTemplate tmpl ctx i = do
  meta <- runContext ctx i
  applyToMetadata tmpl meta >>= \case
    Right a -> return $ i { itemBody = a }
    Left err -> fail err

loadTemplate :: forall tmpl m. (MonadSake m, Templatable tmpl) => FilePath -> m (Item tmpl)
loadTemplate fp = do
  i <- loadItem fp
  eith <- compileTemplate (itemIdentifier i) (itemBody i)
  case eith of
    Right t  -> return $ i { itemBody = t}
    Left err -> error err

loadAndApplyTemplate :: forall tmpl proxy a m.
                        (MonadSake m, Templatable tmpl)
                     => proxy tmpl     -- ^ Template
                     -> FilePath       -- ^ Template identifier
                     -> Context a      -- ^ Context
                     -> Item a         -- ^ Page
                     -> m (Item Text)  -- ^ Resulting item
loadAndApplyTemplate _ identifier context item = do
  tpl <- itemBody <$> loadTemplate @tmpl identifier
  applyTemplate tpl context item

field :: ToJSON a => String -> (forall m. MonadSake m => Item b -> m a) -> Context b
field k mk = Context $ fmap (HM.singleton (T.pack k) . toJSON) . mk

field_ :: ToJSON a1 => String -> (Item a2 -> a1) -> Context a2
field_ k mk = Context $ return . HM.singleton (T.pack k) . toJSON . mk

constField :: ToJSON a => String -> a -> Context b
constField k v =
  Context $ const $ return $
  HM.singleton (T.pack k) $ toJSON v

objectContext :: ToJSON a => a -> Context b
objectContext v = Context $ \_ ->
  case toJSON v of
    Object dic -> return dic
    _          -> error $ "Should be object, but got: " ++ LT.unpack (LT.decodeUtf8 (encode v))


bodyField :: ToJSON a => String -> Context a
bodyField key = field key $ return . itemBody

pathField :: String -> Context a
pathField key = field key $ return . runIdentifier . itemIdentifier

defaultContext :: ToJSON a => Context a
defaultContext =
  mconcat [ bodyField "body", metadataField, pathField "path", titleField "title"]

titleField :: String -> Context a
titleField key = field key $ return . takeBaseName . runIdentifier . itemIdentifier

listField :: String -> Context a -> [Item a] -> Context b
listField key chCtx is = field key $ \ _ -> mapM (runContext chCtx) is

applyTemplateList :: (Templatable tmpl, MonadSake m) => tmpl -> Context a -> [Item a] -> m Text
applyTemplateList = applyJoinTemplateList ""

applyJoinTemplateList :: (MonadSake m, Templatable tmpl)
                      => Text -> tmpl -> Context a -> [Item a] -> m Text
applyJoinTemplateList sep tmpl cxt is =
  either error (T.intercalate sep) . sequence
    <$> mapM (applyToMetadata tmpl <=< runContext cxt) is

-- | Dynamic field, built from variables in scope.
--
-- @
-- applyTemplate tmpl $(dynField ['foo, 'bar, 'baz])
-- @
dynField :: [Name] -> ExpQ
dynField names =
  let dic = listE
            [ [| (T.pack $(litE $ stringL $ nameBase n), toJSON $(varE n)) |]
            | n <- names]
  in [| HM.fromList $(dic) |]
