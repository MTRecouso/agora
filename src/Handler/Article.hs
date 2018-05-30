{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Article where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Data.List ((!!))
import Data.Text.Encoding as TE
import Data.CaseInsensitive as CI
import Prelude (read)

postArticleR :: Handler Value
postArticleR = do
    article <- requireJsonBody :: Handler Article
    aId <- runDB $ insert article
    sendStatusJSON created201 (object["id" .= fromSqlKey aId])


getArticlesToUser :: Handler Html
getArticlesToUser = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                    (Just id) -> do
                        return id
                    _ -> do
                        redirect LoginPageR
    userId <- return $ toSqlKey (read (unpack idText) ::Int64)
    user <- runDB $ get404 userId
    views <- runDB $ selectList [ViewUser ==. userId] []
    viewarticles <- return $ fmap(\view -> viewArticle $ entityVal view) views
    viewedArticles  <- runDB $ selectList [ArticleId <-.viewarticles] [LimitTo 30]
    tagsByFrequence <- return $ sort $ fmap (\tag -> articleTag $ entityVal tag) viewedArticles
    tagsDesc <- return $ reverse $ (sortOn length (group tagsByFrequence))
    mostViewedTags <- return $ fmap(\x -> x !! 0) $ take 3 tagsDesc 
    pageArticles <- runDB $ selectList [ArticleTag <-. mostViewedTags] [LimitTo 10]
    pc <- return $ $(widgetFile "mainpage")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layout")

-- Stub for future form route
getArticleR :: Handler Value
getArticleR = do
    sendStatusJSON noContent204 (object[])

getArticleByIdR :: ArticleId -> Handler Html
getArticleByIdR artId = do
    article <- runDB $ get404 artId 
    author <- (runDB $ selectFirst [UserSyId ==. (articleAuthor article)] [])
    authorName <- return (fmap (\x -> userSyUsername $ entityVal x) author) 
    listArticleReactions <- runDB $ selectList [ReactionArticle ==. artId] []
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
    listArticlesReactions <- runDB $ selectList [ViewArticle <-. artIds] []
    listArticlesViews <- runDB $ selectList [ViewArticle <-. artIds] []
    --to do: change navbar routes when user session is implemented
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
    listArticlesReactions <- runDB $ selectList [ViewArticle <-. artIds] []
    listArticlesViews <- runDB $ selectList [ViewArticle <-. artIds] []
    pc <-return $ $(widgetFile "author")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layout")

--getArticleByTag :: TagId -> Handler Value
--May add this in the future if the tag feature gets implemented

putArticleByIdR :: ArticleId -> Handler Value
putArticleByIdR artId = do
    article <- requireJsonBody :: Handler Article
    runDB $ replace artId article
    sendStatusJSON noContent204 (object[])

deleteArticleByIdR :: ArticleId -> Handler Value
deleteArticleByIdR artId = do
    runDB $ deleteCascade artId
    sendStatusJSON noContent204 (object[])