{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Provides @'Templatable'@ instance and utility functions for doctemplates' @'Template'@,
   which is mainly used by Pandoc.
-}
module Web.Sake.Template.DocTemplates
  ( applyAsDocTemplates,
    loadAndApplyDocTemplates,
    DocTemplates,
    module Web.Sake.Template,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Text.DocLayout as DL
import Text.DocTemplates (Template)
import qualified Text.DocTemplates as DT
import Web.Sake.Class
import Web.Sake.Identifier (runIdentifier)
import Web.Sake.Item
import Web.Sake.Template

type DocTemplates = Template Text

instance Templatable DocTemplates where
  compileTemplate ident src =
    liftIO $ DT.compileTemplate (runIdentifier ident) src
  applyToMetadata tmpl meta =
    return $ Right $ DL.render Nothing $ DT.renderTemplate tmpl $ J.Object meta

applyAsDocTemplates :: MonadSake m => Context Text -> Item Text -> m (Item Text)
applyAsDocTemplates = applyAsTemplate' (Nothing :: Maybe (Template Text))

loadAndApplyDocTemplates :: MonadSake m => FilePath -> Context a -> Item a -> m (Item Text)
loadAndApplyDocTemplates = loadAndApplyTemplate (Nothing :: Maybe DocTemplates)
