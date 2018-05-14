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
    vid <- runDB $ insert view
    sendStatusJSON created201 (object["resp" .= fromSqlKey vid])


getViewByIdR :: ViewId -> Handler Value
getViewByIdR viewId = do
    view <- runDB $ get404 viewId
    user <-(runDB $ selectFirst [UserSyId ==. (viewUser view)] [])
    userName <- return (fmap (\x -> userSyUsername $ entityVal x) user) 
    article <-(runDB $ selectFirst [ArticleId ==. (viewArticle view)] [])
    articleName <- return (fmap (\x -> articleTitle $ entityVal x) article)
    sendStatusJSON ok200 (object["resp" .= (view, userName, articleName)])