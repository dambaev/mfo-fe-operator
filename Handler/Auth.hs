{-#LANGUAGE BangPatterns#-}
{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE OverloadedStrings#-}
module Handler.Auth where

import Yesod
import Yesod.Auth
import Prelude
import Codec.JSON.RPC
import Data.Text as T
import Codec.MFO.API.Operator
import Data.Aeson
import Data.ByteString.UTF8 as BU8
import Data.Text as T
import Data.Text.Encoding as E
import Data.String as DS


baseURL = "http://gw-operator/"

authGW:: AuthPlugin site
authGW = AuthPlugin "gw" dispatch getLoginR
    where
    dispatch "POST" ["login"] = checkLogin >>= sendResponse
    dispatch "GET" ["checkpin"] = getCheckPinR >>= sendResponse
    dispatch "POST" ["checkpin"] = checkPin >>= sendResponse
    dispatch _ _ = notFound

loginR:: AuthRoute
loginR = PluginR "gw" ["login"]
    
getLoginR:: (Route Auth -> Route master)-> WidgetT master IO ()
getLoginR tm = do
    [whamlet|
<form method="POST" action=@{tm loginR}>
    <table>
        <tr>
            <th>Имя пользователя:
            <td>
                <input type="text" name="login" required>
        <tr>
            <th>Пароль:
            <td>
                <input type="password" name="password" required>
        <tr>
            <td>
                <button type=submit>
                    Войти
|]

getCheckPinR:: YesodAuth site => HandlerT Auth (HandlerT site IO) Html
getCheckPinR = do
    tm <- getRouteToParent
    lift $ authLayout $ [whamlet|
<form method="POST" action=@{tm checkPinR}>
    <table>
        <tr>
            <th>Введите PIN-код для подтвержения:
            <td>
                <input type="text" name="pin" required>
        <tr>
            <td>
                <button type=submit>
                    Войти
|]

checkLogin:: YesodAuth site => HandlerT Auth (HandlerT site IO) Html
checkLogin = do
    clearSession
    mlogin <- lookupPostParam "login"
    mpassword <- lookupPostParam "password"
    case (mlogin, mpassword) of
         (_, Nothing) -> badMethod
         (Nothing, _) -> badMethod
         (Just login, Just password) -> do
             liftIO $! print $! show $! toJSON $! ORALogin login password
             ret <- jsonCall (baseURL `T.append` "auth/page/mfo/login")
                 $! ORALogin login password
             liftIO $! print $! "ret = " ++ show ret
             case ret of
                  Error reason -> notFound
                  Success OAANeedPin -> redirect $! checkPinR
                  _ -> notFound


checkPinR:: AuthRoute
checkPinR = PluginR "gw" ["checkpin"]


checkPin:: YesodAuth site => HandlerT Auth (HandlerT site IO) Html
checkPin = do
    mpin <- lookupPostParam "pin"
    case mpin of
         Nothing -> do
             setMessage "Вы не ввели PIN-код!"
             redirect $ checkPinR
         Just pin -> do
             ret <- jsonCall (baseURL `T.append` "auth/page/mfo/verify")
                 $! ORAPin pin
             case ret of
                Error e -> do
                      setMessage $ DS.fromString $ "ошибка подтверждения PIN-кода, " 
                          ++ show e
                      redirect $ loginR
                Success OAALoggedIn {..} -> do
                    lift $ setCreds True $ Creds "gw" oaaFullName []
                    notFound
                _ -> do
                      setMessage $ "ошибка подтверждения PIN-кода" 
                      redirect $ checkPinR
