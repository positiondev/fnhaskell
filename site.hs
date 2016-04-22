{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "tutorial/README.md" $ do
        route   $ customRoute $ const "tutorial/index.html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "tutorial/*.lhs" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
           >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

config = defaultConfiguration
  { deployCommand = "s3cmd -P sync _site/ s3://fnhaskell.com"
    }
