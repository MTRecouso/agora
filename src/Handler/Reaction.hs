{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Reaction where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postReactionR :: Handler Value
postReactionR = do
    reaction <- requireJsonBody :: Handler Reaction
    rid <- runDB $ insert reaction
    sendStatusJSON created201 (object["id" .= fromSqlKey rid])


getReactionByIdR :: ReactionId -> Handler Value
getReactionByIdR rId = do
    reaction <- runDB $ get404 rId
    rtype <- (runDB $ selectFirst [RTypeId ==. (reactionType reaction)] [])
    user <-(runDB $ selectFirst [UserSyId ==. (reactionUser reaction)] [])
    userName <- return (fmap (\x -> userSyUsername $ entityVal x) user) 
    article <-(runDB $ selectFirst [ArticleId ==. (reactionArticle reaction)] [])
    articleName <- return (fmap (\x -> articleTitle $ entityVal x) article)
    sendStatusJSON ok200 (object["reaction" .= (reaction, rtype, userName, articleName)])