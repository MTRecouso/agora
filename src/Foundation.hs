{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just LoginPageR
    isAuthorized LoginPageR _ = return Authorized
    isAuthorized SignupPageR _ = return Authorized
    isAuthorized UserSyR _ = return Authorized
    isAuthorized UserLoginR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized _ _ = ehUsuario

ehUsuario :: Handler AuthResult
ehUsuario = do
    maybeId <- lookupSession "ID"
    case maybeId of
        Nothing -> return AuthenticationRequired 
        Just _ -> return Authorized
    
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
