module Web.Sake
    ( module Web.Sake.Class
    , module Web.Sake.Identifier
    , module Web.Sake.Item
    , module Web.Sake.Metadata
    , module Web.Sake.Template
    , module Web.Sake.Template.DocTemplates
    , module Web.Sake.Template.Mustache
    , -- * External Modules
      module Development.Shake
    ) where
import Web.Sake.Class
import Web.Sake.Identifier
import Web.Sake.Item
import Web.Sake.Metadata
import Web.Sake.Template
import Web.Sake.Template.DocTemplates
import Web.Sake.Template.Mustache

import Development.Shake hiding (copyFile', need, needed, putLoud, putNormal,
                          putQuiet, readFile')
