{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.User where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Crypto.BCrypt


postUserSyR :: Handler Value
postUserSyR = do
    user <- requireJsonBody :: Handler UserSy
    (Just hashedPass) <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack $ unpack $ userSyPassword user)
    hashUser <- return $ UserSy (userSyEmail user) (pack $ BS.unpack hashedPass) (userSyUsername user)
    userId <- runDB $ insert hashUser
    sendStatusJSON created201 (object ["resp" .= (userId)])


postUserLoginR :: Handler Html
postUserLoginR = do
    (email,password) <- requireJsonBody :: Handler (Text,Text)
    maybeUser <- runDB $ getBy $ UniqueEmail email
    case maybeUser of
        Just user -> do
            loginAttempt <- return $ validatePassword (BS.pack $ unpack $ userSyPassword $ entityVal user) (BS.pack $ unpack password)
            case loginAttempt of
                True -> do 
                    setSession "ID" $ pack $ show $ fromSqlKey $ entityKey user
                    redirect ArticlesToUser
                _ -> sendStatusJSON ok200 (object ["resp" .= ("Login não autorizado" :: Text)] )
        _ -> sendStatusJSON status404 (object ["resp" .= ("Usuario não cadastrado" :: Text)] )


getUserSyByIdR :: UserSyId -> Handler Value
getUserSyByIdR uId = do
    user <- runDB $ get404 uId
    userName <-  return $ userSyUsername user
    userEmail <- return $ userSyEmail user
    sendStatusJSON ok200 (object["user" .= (userName, userEmail)])
    
    