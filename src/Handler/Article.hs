{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Article where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postArticleR :: Handler Value
postArticleR = do
    article <- requireJsonBody :: Handler Article
    aid <- runDB $ insert article
    sendStatusJSON created201 (object["resp" .= fromSqlKey aid])


getArticleByIdR :: ArticleId -> Handler Value
getArticleByIdR artId = do
    article <- runDB $ get404 artId 
    author <- (runDB $ selectFirst [UserSyId ==. (articleAuthor article)] [])
    authorName <- return (fmap (\x -> userSyUsername $ entityVal x) author) 
    listArticleReactions <- runDB $ selectList [ReactionArticle ==. artId] []
    listArticleViews <- runDB $ selectList [ViewArticle ==. artId] []
    sendStatusJSON ok200 (object["resp" .= (article, authorName, listArticleViews, listArticleReactions)])


getArticleByAuthor :: UserSyId -> Handler Value
getArticleByAuthor authorId = do
    articles <- runDB $ selectList [ArticleAuthor ==. authorId] []
    artIds <- return $ fmap(\article -> entityKey article) articles
    listArticlesReactions <- runDB $ selectList [ViewArticle <-. artIds] []
    listArticlesViews <- runDB $ selectList [ViewArticle <-. artIds] []
    sendStatusJSON ok200 (object["resp" .= (articles, listArticlesReactions, listArticlesViews)])




putArticleByIdR :: ArticleId -> Handler Value
putArticleByIdR artId = do
    article <- requireJsonBody :: Handler Article
    runDB $ replace artId article
    sendStatusJSON noContent204 (object[])

deleteArticleByIdR :: ArticleId -> Handler Value
deleteArticleByIdR artId = do
    runDB $ deleteCascade artId
    sendStatusJSON noContent204 (object[])