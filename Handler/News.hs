{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RecordWildCards #-}
module Handler.News where

import Import

import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )
import Codec.MFO.API.Operator
import Foundation
import Yesod.Auth
import Codec.JSON.RPC

import Data.Text as T
import Data.Aeson
import Data.List as L

baseURL = "http://gw-operator/"

postAddNewsR:: Handler Html
postAddNewsR = do
    ((result,widget), enctype) <- runFormPost addNewsForm
    case result of
        FormSuccess news -> do
            liftIO $! print $ news
            ret <- jsonCall (baseURL `T.append` "news") $! news
            case ret of
                Error some -> do
                    liftIO $! print $! "ret = " ++ show  ret
                    notFound
                Success OANAdded -> do
                    setMessage "added"
                    redirect HomeR
                Success err -> do
                    defaultLayout $! do
                        [whamlet|#{show err}|]
        FormFailure desc ->
            defaultLayout $! [whamlet|#{show desc}|]
        _ -> defaultLayout [whamlet|
<p>
    Неверные данные
|]

addNewsForm:: Html-> MForm Handler (FormResult OReqNews, Widget)
addNewsForm = renderDivs $ ORNAddNews
    <$> areq textField "title" Nothing
    <*> areq textField "shortContent" Nothing
    <*> areq textField "content" Nothing


postDeleteNewsR:: Handler Html
postDeleteNewsR = undefined

deleteNewsForm:: [NewsShort]-> Html -> MForm Handler (FormResult OReqNews, Widget)
deleteNewsForm posts = do
    let ids = mkOptionList $! L.map (\post -> toOption $! nsId post) posts
        toOption:: Text-> Option Text
        toOption id = Option "" id id
    renderTable $ ORNDeleteNews
        <$> areq (checkboxesField (return ids)) "ids" Nothing


getGetNewsR:: Text-> Handler Html
getGetNewsR id = undefined

