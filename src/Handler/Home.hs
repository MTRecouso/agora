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
    author <- (runDB $ selectFirst [UserSyId ==. (articleAuthor article)] [])
    authorName <- return (fmap (\x -> userSyUsername $ entityVal x) author) 
    listArticleReactions <- runDB $ selectList [ReactionArticle ==. articleId] []
    listArticleViews <- runDB $ selectList [ViewArticle ==. articleId] []
    sendStatusJSON ok200 (object["resp" .= (article, authorName, listArticleViews, listArticleReactions)])

postReactionR :: Handler Value
postReactionR = do
    reaction <- requireJsonBody :: Handler Reaction
    rid <- runDB $ insert reaction
    sendStatusJSON created201 (object["resp" .= fromSqlKey rid])


getReactionByIdR :: ReactionId -> Handler Value
getReactionByIdR reactionId = do
    reaction <- runDB $ get404 reactionId
    rtype <- (runDB $ selectFirst [RTypeId ==. (reactionType reaction)] [])
    user <-(runDB $ selectFirst [UserSyId ==. (reactionUser reaction)] [])
    userName <- return (fmap (\x -> userSyUsername $ entityVal x) user) 
    article <-(runDB $ selectFirst [ArticleId ==. (reactionArticle reaction)] [])
    articleName <- return (fmap (\x -> articleTitle $ entityVal x) article)
    sendStatusJSON ok200 (object["resp" .= (reaction, rtype, userName, articleName)])


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