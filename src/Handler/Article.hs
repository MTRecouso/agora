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
getArticleByIdR articleId = do
    article <- runDB $ get404 articleId 
    author <- (runDB $ selectFirst [UserSyId ==. (articleAuthor article)] [])
    authorName <- return (fmap (\x -> userSyUsername $ entityVal x) author) 
    listArticleReactions <- runDB $ selectList [ReactionArticle ==. articleId] []
    listArticleViews <- runDB $ selectList [ViewArticle ==. articleId] []
    sendStatusJSON ok200 (object["resp" .= (article, authorName, listArticleViews, listArticleReactions)])

putArticleByIdR :: ArticleId -> Handler Value
putArticleByIdR articleId = do
    article <- requireJsonBody :: Handler Article
    runDB $ replace articleId article
    sendStatusJSON noContent204 (object[])