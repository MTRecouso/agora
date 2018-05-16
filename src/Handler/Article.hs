{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Article where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Data.List ((!!))

postArticleR :: Handler Value
postArticleR = do
    article <- requireJsonBody :: Handler Article
    aId <- runDB $ insert article
    sendStatusJSON created201 (object["id" .= fromSqlKey aId])


getArticlesToUser :: UserSyId -> Handler Value
getArticlesToUser userId = do
    views <- runDB $ selectList [ViewUser ==. userId] []
    viewarticles <- return $ fmap(\view -> viewArticle $ entityVal view) views
    viewedArticles  <- runDB $ selectList [ArticleId <-.viewarticles] [LimitTo 30]
    tagsByFrequence <- return $ sort $ fmap (\tag -> articleTag $ entityVal tag) viewedArticles
    tagsDesc <- return $ reverse $ (sortOn length (group tagsByFrequence))
    mostViewedTags <- return $ fmap(\x -> x !! 0) $ take 3 tagsDesc 
    pageArticles <- runDB $ selectList [ArticleTag <-. mostViewedTags] [LimitTo 10] 
    sendStatusJSON ok200 (object["articles" .= pageArticles])


getArticleByIdR :: ArticleId -> Handler Value
getArticleByIdR artId = do
    article <- runDB $ get404 artId 
    author <- (runDB $ selectFirst [UserSyId ==. (articleAuthor article)] [])
    authorName <- return (fmap (\x -> userSyUsername $ entityVal x) author) 
    listArticleReactions <- runDB $ selectList [ReactionArticle ==. artId] []
    listArticleViews <- runDB $ selectList [ViewArticle ==. artId] []
    sendStatusJSON ok200 (object["articles" .= (article, authorName, listArticleViews, listArticleReactions)])

like :: EntityField record Text -> Text -> Filter record
like field val = Filter field (Left ("%" ++ val ++ "%")) (BackendSpecificFilter "ILIKE")

getArticleSearchR :: Text -> Handler Value
getArticleSearchR searchText = do
    articles <- runDB $ selectList [ArticleTitle `like` searchText][]
    artIds <- return $ fmap(\article -> entityKey article) articles
    listArticlesReactions <- runDB $ selectList [ViewArticle <-. artIds] []
    listArticlesViews <- runDB $ selectList [ViewArticle <-. artIds] []
    sendStatusJSON ok200 (object["search_articles" .= (articles, listArticlesReactions, listArticlesViews)])

getArticleByAuthorR :: UserSyId -> Handler Value
getArticleByAuthorR authorId = do
    articles <- runDB $ selectList [ArticleAuthor ==. authorId] []
    artIds <- return $ fmap(\article -> entityKey article) articles
    listArticlesReactions <- runDB $ selectList [ViewArticle <-. artIds] []
    listArticlesViews <- runDB $ selectList [ViewArticle <-. artIds] []
    sendStatusJSON ok200 (object["author_articles" .= (articles, listArticlesReactions, listArticlesViews)])

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