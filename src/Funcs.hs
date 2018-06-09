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
import Network.HTTP.Types.Status

keyToText ::  ToBackendKey SqlBackend record => Key record -> Text
keyToText key = pack $ show $ fromSqlKey $ key

textToKey :: ToBackendKey SqlBackend record => Text -> Key record
textToKey idText = toSqlKey (read (unpack idText) :: Int64)

textSnippet :: Text -> Text
textSnippet content = (take 300 content) ++ pack "..."

hasRequiredParameters :: [Maybe Text] -> Bool
hasRequiredParameters params = foldl (\hasReq param -> if isNothing param then hasReq && False else hasReq && True) True params