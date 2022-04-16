{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Provides @'Templatable'@ instance and utility functions for @'HamletTemplate'@,
--   which is mainly used by Pandoc.
module Web.Sake.Template.Hamlet
       ( applyAsHamlet, loadAndApplyHamlet, HamletTemplate,
         module Web.Sake.Template
       ) where
import Web.Sake.Class
import Web.Sake.Item
import Web.Sake.Template

import           Control.Arrow                 (left, (+++))
import           Control.Exception             (SomeException (..))
import           Data.Aeson                    (Value (..))
import qualified Data.Aeson                    as A
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.Foldable                 as F
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map                      as M
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT
import           Text.Blaze.Html.Renderer.Text
import           Text.Hamlet.Runtime           (HamletData, HamletTemplate,
                                                defaultHamletSettings,
                                                parseHamletTemplate,
                                                renderHamletTemplate,
                                                toHamletData)

-- | Accepts any JSON value but @'Object'@ type.
instance Templatable HamletTemplate where
  compileTemplate _ src =
    return $ left showSomeException $ parseHamletTemplate defaultHamletSettings $ T.unpack src
  applyToMetadata tmpl meta =
    return $ (showSomeException +++ (LT.toStrict . renderHtml)) $ do
      v <- left (SomeException . userError) $ traverse valueToHamlet
        $ KM.toHashMapText meta
      renderHamletTemplate tmpl $ M.fromList $ HM.toList v

showSomeException :: SomeException -> String
showSomeException (SomeException exc) = show exc

valueToHamlet :: Value -> Either String HamletData
valueToHamlet (Number i)  = Right $ toHamletData $ T.pack $ show i
valueToHamlet (Array arr) = toHamletData <$> mapM valueToHamlet (F.toList arr)
valueToHamlet (String str) = Right $ toHamletData str
valueToHamlet Null = Right $ toHamletData ("" :: T.Text)
valueToHamlet (Bool b) = Right $ toHamletData b
valueToHamlet v =
  Left $ "Cannot render as Hamlet value: " ++ LT.unpack (LT.decodeUtf8 $ A.encode v)

applyAsHamlet :: MonadSake m => Context Text -> Item Text -> m (Item Text)
applyAsHamlet = applyAsTemplate' (Nothing :: Maybe HamletTemplate)

loadAndApplyHamlet :: MonadSake m => FilePath -> Context a -> Item a -> m (Item Text)
loadAndApplyHamlet = loadAndApplyTemplate (Nothing :: Maybe HamletTemplate)
