{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Article where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql as DB
import Data.List ((!!))
import Data.Text.Encoding as TE
import Data.CaseInsensitive as CI
import Data.Maybe (fromJust)
import Data.Text as T (replace)
import Funcs
import Text.Julius(rawJS)

postArticleR :: Handler Value
postArticleR = do
    maybeTitle <- lookupPostParam "title"
    maybeContent <- lookupPostParam "content"
    maybeTag <- lookupPostParam "tag"
    maybeImage <- lookupFile "image"
    filePath <- case maybeImage of
        (Just img) -> do
            fp <- return $ "static/img/" ++ (unpack $ fileName img)
            liftIO $ fileMove img fp
            return $ unpack $ fileName img
        Nothing -> do
            return "default.jpg" 
    (Just idText) <- lookupSession "ID"
    userId <- return $ textToKey idText
    hasReqParam <- return $ hasRequiredParameters [maybeTag, maybeContent, maybeTitle]
    case hasReqParam of
        False -> do
            invalidArgs $ [(pack "Formato inválido")]
        True -> do
            article <- return $ Article (fromJust maybeTitle) (fromJust maybeContent) userId (textToKey $ fromJust maybeTag) filePath
            aId <- runDB $ insert article
            redirect ArticlesToUser


getArticleR :: Handler Html
getArticleR = do
    tags <- runDB $ selectList [] [Asc TagName]
    (Just idText) <- lookupSession "ID"
    userId <- return $ textToKey idText
    pc <- return $ $(widgetFile "postarticle")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layout")

getArticlesToUser :: Handler Html
getArticlesToUser = do
    (Just idText) <- lookupSession "ID"
    userId <- return $ textToKey idText
    user <- runDB $ get404 userId
    views <- runDB $ selectList [ViewUser ==. userId] []
    viewarticles <- return $ fmap(\view -> viewArticle $ entityVal view) views
    viewedArticles  <- runDB $ selectList [ArticleId <-.viewarticles] [LimitTo 30]
    pageArticles <- case viewedArticles of
        [] -> do
            runDB $ selectList [] [Desc ArticleId, LimitTo 9]

        _ -> do 
            tagsByFrequence <- return $ sort $ fmap (\tag -> articleTag $ entityVal tag) viewedArticles
            tagsDesc <- return $ reverse $ (sortOn length (group tagsByFrequence))
            mostViewedTags <- return $ fmap(\x -> x !! 0) $ take 3 tagsDesc 
            runDB $ selectList [ArticleTag <-. mostViewedTags] [LimitTo 9]
    
    artIds <- return $ fmap(\article -> entityKey article) pageArticles
    listArticlesFavorites <- runDB $ selectList [FavoriteArticle <-. artIds] [Asc FavoriteArticle]
    listArticlesViews <- runDB $ selectList [ViewArticle <-. artIds] [Asc ViewArticle]
    pc <- return $ $(widgetFile "mainpage")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layout")

getArticleByIdR :: ArticleId -> Handler Html
getArticleByIdR artId = do
    (Just idText) <- lookupSession "ID"
    userId <- return $ (textToKey idText :: UserSyId)
    user <- runDB $ get404 userId
    article <- runDB $ get404 artId
    author <- (runDB $ selectFirst [UserSyId ==. (articleAuthor article)] [])
    hasFavorite <- runDB $ getBy (UniqueFav userId artId)
    favoriteId <- return $ maybe "0" (\fav -> show $ fromSqlKey $ entityKey fav) hasFavorite
    listArticleFavorites <- runDB $ selectList [FavoriteArticle ==. artId] []
    listArticleViews <- runDB $ selectList [ViewArticle ==. artId] []
    pc <-return $ $(widgetFile "article")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layout")
  

like :: EntityField record Text -> Text -> Filter record
like field val = Filter field (Left ("%" ++ val ++ "%")) (BackendSpecificFilter "ILIKE")

getArticleSearchR :: Handler Html
getArticleSearchR = do
    searchParam <- lookupGetParam "search"
    searchText <- return $ fromMaybe " " searchParam
    articles <- runDB $ selectList [ArticleTitle `like` searchText][]
    artIds <- return $ fmap(\article -> entityKey article) articles
    listArticlesFavorites <- runDB $ selectList [FavoriteArticle <-. artIds] [Asc FavoriteArticle]
    listArticlesViews <- runDB $ selectList [ViewArticle <-. artIds] [Asc ViewArticle]
    (Just idText) <- lookupSession "ID"
    userId <- return $ textToKey idText
    pc <-return $ $(widgetFile "searchresults")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layout")

getArticleByAuthorR :: UserSyId -> Handler Html
getArticleByAuthorR authorId = do
    author <- runDB $ get404 authorId     
    articles <- runDB $ selectList [ArticleAuthor ==. authorId] []
    artIds <- return $ fmap(\article -> entityKey article) articles
    listArticlesFavorites <- runDB $ selectList [FavoriteArticle <-. artIds] [Asc FavoriteArticle]
    listArticlesViews <- runDB $ selectList [ViewArticle <-. artIds] [Asc ViewArticle]
    (Just idText) <- lookupSession "ID"
    userId <- return $ textToKey idText
    pc <-return $ $(widgetFile "author")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layout")


putArticleByIdR :: ArticleId -> Handler Value
putArticleByIdR artId = do
    article <- requireJsonBody :: Handler Article
    runDB $ DB.replace artId article
    sendStatusJSON noContent204 (object[])

deleteArticleByIdR :: ArticleId -> Handler Value
deleteArticleByIdR artId = do
    (Just idText) <- lookupSession "ID"
    userId <- return $ (textToKey idText :: UserSyId)
    article <- runDB $ get404 artId
    isAuthor <- return $ (fromSqlKey  $ articleAuthor article) == (fromSqlKey userId)
    case isAuthor of
        True -> do
            runDB $ deleteCascade artId
        _ -> do
            sendStatusJSON unauthorized401 (object[])
    sendStatusJSON noContent204 (object[])