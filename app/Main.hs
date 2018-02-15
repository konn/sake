{-# LANGUAGE DeriveAnyClass, ExtendedDefaultRules, MultiWayIf       #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, StandaloneDeriving #-}
{-# LANGUAGE TypeApplications, TypeSynonymInstances                 #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans #-}
module Main where
import Web.Sake

import           Control.Monad                  (filterM, void)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Default
import           Data.Functor.Contravariant
import           Data.Monoid                    ((<>))
import           Data.Store
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.ICU.Normalize
import qualified Data.Text.IO                   as T
import           Development.Shake.FilePath
import           Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp       as Warp
import           Text.Megaparsec                (Pos, mkPos, unPos)
import           Text.Mustache                  (Key, Node, PName)
import           Text.Pandoc
import           WaiAppStatic.Types

default (T.Text, String, Integer)

deriving instance Store Key
instance Store Pos where
  size = contramap unPos size
  poke = poke . unPos
  peek = mkPos <$> peek
deriving instance Store Node
deriving instance Store PName
deriving instance Store Mustache

siteDest, siteSrc, templateDir, cacheDir :: FilePath
siteDest = "_site"
siteSrc  = "src-site"
templateDir = "templates"
cacheDir = "_build" </> "cache"

replaceDir :: FilePath -> FilePath -> FilePath -> FilePath
replaceDir from to pth = to </> makeRelative from pth

template :: FilePath -> FilePath
template fp = "_cache" </> "templates" </> fp

page :: FilePath -> FilePath
page fp = siteDest </> fp

shOpts :: ShakeOptions
shOpts = shakeOptions { shakeFiles = "_build"
                      , shakeChange = ChangeModtimeOrDigest
                      }

mathField :: Context a
mathField = field "math" $ \i ->
  return $ "//*.tex" ?== runIdentifier (itemIdentifier i)

main :: IO ()
main = shakeArgs shOpts $ do
  phony "watch" $ liftIO $ do
      void $
        Warp.run 8000 (staticApp $ (defaultFileServerSettings siteDest) {ssIndices  = [unsafeToPiece "index.html"]})

  rules

rules :: Rules ()
rules = do
  want ["site"]

  "site" ~> do
    cmd_ "mkdir" "-p" siteDest
    mds   <- getDirectoryFiles siteSrc ["//*.md"]
    texs  <- getDirectoryFiles siteSrc ["//*.tex"]
    htmls <- getDirectoryFiles siteSrc ["//*.html"]
    copies <- getDirectoryFiles siteSrc ["css//*", "js//*", "img//*"]
    let genHtmls = map (-<.> "html") (mds ++ texs) ++ htmls
        genPDFs  = map (-<.> "pdf") texs
    need $ map (siteDest </>) $ genHtmls ++ genPDFs ++ copies

  "_cache" </> "templates" <//> "*.mustache" %> \out -> do
    copyFile' (dropDirectory1 out) out

  (siteDest <//> "*.html") %> \out -> do
    let origBase = replaceDir siteDest siteSrc out
        mdVer    = origBase -<.> "md"
        texVer   = origBase -<.> "tex"
        htmlVer  = origBase -<.> "html"
    sources <- filterM doesFileExist [mdVer, texVer, htmlVer]
    if null sources
      then error $ "No source for " ++ show out ++ " found."
      else do
      let fp = head sources
      putNormal $ "Compiling " ++ show fp ++ " to generate " ++ show out
      let cxt = mconcat [ constField "siteName" "Sake Example"
                        , constField "host" "https://konn.github.io/sake"
                        , mathField
                        , defaultContext
                        ]
      src <- loadAndApplyMustache (template "default.mustache") cxt
             =<< compilePandoc
                    def
                    def { writerHTMLMathMethod = KaTeX "//cdnjs.cloudflare.com/ajax/libs/KaTeX/0.8.3/contrib/auto-render.min.js" }
             =<< applyAsMustache cxt . changeMustache
             =<< loadItem fp
      writeTextFile out $ normalize NFC $ itemBody src
      putQuiet $ "Generated: " ++ makeRelative siteDest out

  (siteDest <//> "*.pdf") %> \out -> do
    let origFile = replaceDir siteDest siteSrc out
        fp       = origFile -<.> "tex"
    putNormal $ "Compiling " ++ show fp ++ " to generate " ++ show out
    i <- applyAsMustache defaultContext . changeMustache =<< loadItem fp
    withTempDir $ \tDir -> do
      let texDest = tDir </> takeFileName fp
          targ = takeFileName fp
      writeTextFile texDest $ itemBody i
      cmd_ (Cwd tDir) (EchoStdout False) (EchoStderr False) "latexmk" "-pdflua" targ
      cmd_ "mkdir" "-p" $ takeDirectory1 out
      cmd_ "cp" (texDest -<.> "pdf") out
    putQuiet $ "Generated: " ++ makeRelative siteDest out

  -- If no rule is applicable, then try to copy as-is.
  priority 0 $ (siteDest <//> "*") %> \out ->
    copyFile' (replaceDir siteDest siteSrc out) out

changeMustache :: Item Text -> Item Text
changeMustache i@Item{..}
  | "//*.tex" ?== runIdentifier itemIdentifier = i { itemBody = "{{=\\template{ }=}}" <> itemBody }
  | otherwise = i

