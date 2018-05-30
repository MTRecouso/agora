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
--import Yesod.Auth.HashDB (setPassword,userPasswordHash, setPasswordHash, validatePass, HashDBUser)
--import Yesod.Auth.Util.PasswordStore (makePassword, strengthenPassword,
                                             -- verifyPassword, passwordStrength)



postUserSyR :: Handler Value
postUserSyR = do
    user <- requireJsonBody :: Handler UserSy
    (Just hashedPass) <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack $ unpack $ userSyPassword user)
    hashUser <- return $ UserSy (userSyEmail user) (pack $ BS.unpack hashedPass) (userSyUsername user)
    userId <- runDB $ insert hashUser
    sendStatusJSON created201 (object ["resp" .= (userId)])

getUserSyByIdR :: UserSyId -> Handler Value
getUserSyByIdR uId = do
    user <- runDB $ get404 uId
    userName <-  return $ userSyUsername user
    userEmail <- return $ userSyEmail user
    sendStatusJSON ok200 (object["user" .= (userName, userEmail)])
    
    