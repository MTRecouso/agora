{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.View where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postViewR :: Handler Value
postViewR = do
    view <- requireJsonBody :: Handler View
    vId <- runDB $ insert view
    sendStatusJSON created201 (object["resp" .= fromSqlKey vId])


getViewByIdR :: ViewId -> Handler Value
getViewByIdR vId = do
    view <- runDB $ get404 vId
    user <-(runDB $ selectFirst [UserSyId ==. (viewUser view)] [])
    userName <- return (fmap (\x -> userSyUsername $ entityVal x) user) 
    article <-(runDB $ selectFirst [ArticleId ==. (viewArticle view)] [])
    articleName <- return (fmap (\x -> articleTitle $ entityVal x) article)
    sendStatusJSON ok200 (object["resp" .= (view, userName, articleName)])

getViewByUser :: UserSyId -> Handler Value
getViewByUser uId = do
    views <- runDB $ selectList [ViewUser ==. uId] []
    artIds <- return $ fmap(\view -> viewArticle $ entityVal view) views
    listArticles <- runDB $ selectList [ArticleId <-. artIds] []
    sendStatusJSON ok200 (object["author_articles" .= (views, listArticles)])