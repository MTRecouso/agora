{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Favorite where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postFavoriteR :: Handler Value
postFavoriteR = do
    favorite <- requireJsonBody :: Handler Favorite
    fId <- runDB $ insert400 favorite
    sendStatusJSON created201 (object["id" .= fromSqlKey fId])


getFavoriteByIdR :: FavoriteId -> Handler Value
getFavoriteByIdR fId = do
    favorite <- runDB $ get404 fId
    user <-(runDB $ selectFirst [UserSyId ==. (favoriteUser favorite)] [])
    userName <- return (fmap (\x -> userSyUsername $ entityVal x) user) 
    article <-(runDB $ selectFirst [ArticleId ==. (favoriteArticle favorite)] [])
    articleName <- return (fmap (\x -> articleTitle $ entityVal x) article)
    sendStatusJSON ok200 (object["favorite" .= (favorite, userName, articleName)])

putFavoriteByIdR :: FavoriteId -> Handler Value
putFavoriteByIdR fId = do
    favorite <- requireJsonBody :: Handler Favorite
    runDB $ replace fId favorite
    sendStatusJSON noContent204 (object[])

deleteFavoriteByIdR :: FavoriteId  -> Handler Value
deleteFavoriteByIdR fId = do
    runDB $ delete fId
    sendStatusJSON noContent204 (object[])