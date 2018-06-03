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
import Data.Maybe (fromJust)
import Crypto.BCrypt
import Funcs

postUserSyR :: Handler Value
postUserSyR = do
    maybeEmail<- lookupPostParam "email"
    maybePassword <- lookupPostParam "password"
    maybeUsername <- lookupPostParam "username"
    hasReqParam <- return $ hasRequiredParameters [maybeEmail, maybePassword, maybeUsername]
    case hasReqParam of
        False -> do
            sendStatusJSON status404 (object ["resp" .= ("Formato inválido" :: Text)] )
        True -> do
            (Just hashedPass) <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack $ unpack $ fromJust maybePassword)
            hashUser <- return $ UserSy (fromJust maybeEmail) (pack $ BS.unpack hashedPass) (fromJust maybeUsername)
            userId <- runDB $ insert hashUser
            setSession "ID" $ keyToText $ userId
            redirect ArticlesToUser

postUserLoginR :: Handler Value
postUserLoginR = do
    maybeEmail<- lookupPostParam "email"
    maybePassword <- lookupPostParam "password"
    hasReqParam <- return $ hasRequiredParameters [maybeEmail, maybePassword]
    case hasReqParam of
        False -> do
            sendStatusJSON status404 (object ["resp" .= ("Formato inválido" :: Text)] )
        True -> do
            maybeUser <- runDB $ getBy $ UniqueEmail $ fromJust maybeEmail
            case maybeUser of
                Just user -> do
                    loginAttempt <- return $ validatePassword (BS.pack $ unpack $ userSyPassword $ entityVal user) (BS.pack $ unpack $ fromJust maybePassword)
                    case loginAttempt of
                        True -> do 
                            setSession "ID" $ keyToText $ entityKey user
                            redirect ArticlesToUser
                        _ -> sendStatusJSON ok200 (object ["resp" .= ("Login não autorizado" :: Text)] )
                _ -> sendStatusJSON status404 (object ["resp" .= ("Usuario não cadastrado" :: Text)] )
    

getLoginPageR :: Handler Html
getLoginPageR = do
    pc <- return $ $(widgetFile "login")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layoutlogin")

getSignupPageR :: Handler Html
getSignupPageR = do
    pc <- return $ $(widgetFile "signup")
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "Ágora"
        $(widgetFile "layoutlogin")
   

getUserSyByIdR :: UserSyId -> Handler Value
getUserSyByIdR uId = do
    user <- runDB $ get404 uId
    userName <-  return $ userSyUsername user
    userEmail <- return $ userSyEmail user
    sendStatusJSON ok200 (object["user" .= (userName, userEmail)])
    
    