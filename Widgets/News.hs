{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ScopedTypeVariables#-}
module Widgets.News
    ( newsW
    )
    where

import Import

import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

import Text.Julius (rawJS)
import Handler.News (addNewsForm,deleteNewsForm)
import Codec.JSON.RPC
import Data.Conduit as C
import Data.Conduit.List as CL
import Data.ByteString.Char8 as C8
import Codec.MFO.API.Operator
import Data.Text as T
import Control.Monad.Trans.Resource
import Data.Conduit.Binary as CB
import Control.Exception as E (IOException(..))
import Data.Aeson

baseURL = "http://gw-operator/"

newsW::Widget
newsW = do
    addScriptRemote "https://cdn.ckeditor.com/4.5.2/standard/ckeditor.js"
    mainDivI <- newIdent
    addButtonI <- newIdent
    headerDivI <- newIdent
    titleI <- newIdent
    shortContentI <- newIdent
    contentI <- newIdent
    submitI <- newIdent
    addNewsFormI <- newIdent
    newsListHeaderDivI <- newIdent
    newListDivI <- newIdent
    newsItemPostdateI <- newIdent
    tagsI <- newIdent
    deleteNewsFormI <- newIdent

    shortPosts <- handlerToWidget $ getShortPosts
    (addNewsFormW, addNewsFormE) <- handlerToWidget $ generateFormPost addNewsForm
    (deleteNewsFormW, deleteNewsFormE) <- handlerToWidget $ generateFormPost $! deleteNewsForm shortPosts
    $(widgetFile "utils/jspost")
    $(widgetFile "news/news")

getShortPosts:: Handler [NewsShort]
getShortPosts = do
    let toNewsShort:: Monad m => Conduit ByteString m NewsShort
        toNewsShort = do
            await >>= maybe (return ()) conv 
            where
            conv str =  do
                case decodeStrict' str of
                    Nothing -> toNewsShort
                    Just some -> do
                        yield some
                        toNewsShort
    res <- callGet (baseURL `T.append` "news" ) $! CB.lines =$= toNewsShort =$= CL.consume
    return res

