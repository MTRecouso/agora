{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Funcs where

import Yesod
import Foundation
import Import
import Database.Persist.Postgresql
import Prelude (read, foldl)
import Data.List (elemIndices)
import Network.HTTP.Types.Status

keyToText ::  ToBackendKey SqlBackend record => Key record -> Text
keyToText key = pack $ show $ fromSqlKey $ key

textToKey :: ToBackendKey SqlBackend record => Text -> Key record
textToKey idText = toSqlKey (read (unpack idText) :: Int64)

textSnippet :: Text -> Text
textSnippet content = (take 300 content) ++ pack "..."

articleFavs :: Entity Article -> [Entity Favorite] -> [Int]
articleFavs article favs =  elemIndices (fromSqlKey $ entityKey article) (map(\fav -> fromSqlKey $ favoriteArticle $ entityVal fav) favs)

articleViews :: Entity Article -> [Entity View] -> [Int]
articleViews article views =  elemIndices (fromSqlKey $ entityKey article) (map(\view -> fromSqlKey $ viewArticle $ entityVal view) views)

hasRequiredParameters :: [Maybe Text] -> Bool
hasRequiredParameters params = foldl (\hasReq param -> if isNothing param then hasReq && False else hasReq && True) True params