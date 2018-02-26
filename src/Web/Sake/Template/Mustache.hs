{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Provides @'Templatable'@ instance and utility functions for mustache @'Template'@s.
module Web.Sake.Template.Mustache
       (applyAsMustache, loadAndApplyMustache, Mustache
       , module Web.Sake.Template
       ) where
import Web.Sake.Class
import Web.Sake.Identifier
import Web.Sake.Item
import Web.Sake.Template

import           Control.Arrow   (left)
import           Data.Aeson      (toJSON)
import           Data.String     (IsString (fromString))
import           Data.Text       (Text)
import qualified Data.Text.Lazy  as LT
import           Text.Megaparsec (parseErrorPretty')
import           Text.Mustache   (Template, compileMustacheFile,
                                  compileMustacheText, renderMustache)

-- | Synonym for mustache's @'Template'@, to avoid collision
--   between other engine's @'Template'@.
type Mustache = Template

instance Templatable Mustache where
  compileTemplate ident src =
    return $ left (parseErrorPretty' src) $ compileMustacheText (fromString $ runIdentifier ident) src
  applyToMetadata tmpl meta =
    return $ Right $ LT.toStrict $ renderMustache tmpl $ toJSON meta

instance Readable Mustache where
  readFrom_ = compileMustacheFile

applyAsMustache :: MonadSake m => Context Text -> Item Text -> m (Item Text)
applyAsMustache = applyAsTemplate' (Nothing :: Maybe Template)

loadAndApplyMustache :: MonadSake m => FilePath -> Context a -> Item a -> m (Item Text)
loadAndApplyMustache = loadAndApplyTemplate (Nothing :: Maybe Mustache)
