{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = undefined


postUserSyR :: Handler Value
postUserSyR = do
    user <- requireJsonBody :: Handler UserSy
    uid <- runDB $ insert user
    sendStatusJSON created201 (object["resp" .= fromSqlKey uid])


postArticleR :: Handler Value
postArticleR = do
    article <- requireJsonBody :: Handler Article
    aid <- runDB $ insert article
    sendStatusJSON created201 (object["resp" .= fromSqlKey aid])


getArticleByIdR :: ArticleId -> Handler Value
getArticleByIdR articleId = do
    article <- runDB $ get404 articleId
    listArticleReactions <- runDB $ selectList [ReactionArticle ==. articleId] []
    listArticleViews <- runDB $ selectList [ViewArticle ==. articleId] []
    sendStatusJSON ok200 (object["resp" .= (article, listArticleViews, listArticleReactions)])