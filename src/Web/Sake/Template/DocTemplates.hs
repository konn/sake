{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances                                        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Provides @'Templatable'@ instance and utility functions for doctemplates' @'Template'@,
--   which is mainly used by Pandoc.
module Web.Sake.Template.DocTemplates
       (applyAsDocTemplates, loadAndApplyDocTemplates, DocTemplates
       ) where
import Web.Sake.Item
import Web.Sake.Template

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text              (Text)
import           Text.DocTemplates      (Template)
import qualified Text.DocTemplates      as DT


type DocTemplates = Template

instance Templatable DocTemplates where
  compileTemplate _ src =
    return $ DT.compileTemplate src
  applyToMetadata tmpl meta =
    return $ Right $ DT.renderTemplate tmpl meta

applyAsDocTemplates :: MonadIO m => Context Text -> Item Text -> m (Item Text)
applyAsDocTemplates = applyAsTemplate' (Nothing :: Maybe Template)

loadAndApplyDocTemplates :: MonadIO m => FilePath -> Context a -> Item a -> m (Item Text)
loadAndApplyDocTemplates = loadAndApplyTemplate @DocTemplates Nothing
