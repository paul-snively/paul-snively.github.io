--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import           Data.Monoid (mappend)
import           Hakyll
import           Control.Monad (liftM)
import           System.FilePath               (takeBaseName)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "error/*" $ do
        route $ (gsubRoute "error/" (const "") `composeRoutes` setExtension "html")
        compile $ pandocCompiler
            >>= applyAsTemplate siteCtx
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)

    match "pages/*" $ do
        route $ setExtension "html"
        compile $ do
            pageName <- takeBaseName . toFilePath <$> getUnderlying
            let pageCtx = constField pageName "" `mappend`
                          baseNodeCtx
            let evalCtx = functionField "get-meta" getMetadataKey `mappend`
                          functionField "eval" (evalCtxKey pageCtx)
            let activeSidebarCtx = sidebarCtx (evalCtx <> pageCtx)

            pandocCompiler
                >>= saveSnapshot "page-content"
                >>= loadAndApplyTemplate "templates/page.html"    siteCtx
                >>= loadAndApplyTemplate "templates/default.html" (activeSidebarCtx <> siteCtx)
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "archive" "" `mappend`
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> archiveCtx)
                >>= relativizeUrls

    paginate <- buildPaginateWith postsGrouper "posts/*" postsPageId

    paginateRules paginate $ \page pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pattern "content"
            let indexCtx =
                    constField "title" (if page == 1 then "Home"
                                                     else "Blog posts, page " ++ show page) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "home" "" `mappend`
                    paginateContext paginate page `mappend`
                    siteCtx

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend`
                    bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom feedConfig feedCtx posts

--------------------------------------------------------------------------------

postsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
postsGrouper = liftM (paginateEvery 3) . sortRecentFirst

postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ if (n == 1) then "index.html" else show n ++ "/index.html"

--------------------------------------------------------------------------------

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Paul Snively's Blog"
    , feedDescription = "Mostly Thoughts on Functional Programming and Types"
    , feedAuthorName  = "Paul Snivnely"
    , feedAuthorEmail = "psnively@mac.com"
    , feedRoot        = "https://paul-snively.github.io"
    }

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

siteCtx :: Context String
siteCtx =
    baseCtx `mappend`
    constField "site_description" "Mostly Thoughts on Functional Programming and Types" `mappend`
    constField "site-url" "https://paul-snively.github.io" `mappend`
    constField "tagline" "I hope functional programming is your type." `mappend`
    constField "site-title" "Paul Snively" `mappend`
    constField "copy-year" "2022" `mappend`
    constField "github-repo" "https://github.com/paul-snively/paul-snively.github.io" `mappend`
    defaultContext

baseCtx =
    constField "baseurl" "http://localhost:8000"

--------------------------------------------------------------------------------

sidebarCtx :: Context String -> Context String
sidebarCtx nodeCtx =
    listField "list_pages" nodeCtx (loadAllSnapshots "pages/*" "page-content") `mappend`
    defaultContext

baseNodeCtx :: Context String
baseNodeCtx =
    urlField "node-url" `mappend`
    titleField "title" `mappend`
    baseCtx

baseSidebarCtx = sidebarCtx baseNodeCtx

evalCtxKey :: Context String -> [String] -> Item String -> Compiler String
evalCtxKey context [key] item = (unContext context key [] item) >>= \cf ->
        case cf of
            StringField s -> return s
            _             -> error $ "Internal error: StringField expected"

getMetadataKey :: [String] -> Item String -> Compiler String
getMetadataKey [key] item = getMetadataField' (itemIdentifier item) key

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
    {   deployCommand = [r|# Temporarily store uncommited changes
                        git stash

                        # Verify correct branch
                        git checkout hakyll

                        # Build new files
                        stack exec site build

                        # Commit
                        git add _site
                        cd ./_site
                        git add -A
                        git commit -m "Publish."

                        # Push
                        git push origin master

                        # Restoration
                        cd ..
                        git stash pop|]
    }
