module Widgets.DefaultHeader 
    ( defaultHeaderW
    )
    where

import Import

import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

defaultHeaderW:: Widget 
defaultHeaderW = do
    defaultHeaderI <- newIdent
    $(widgetFile "default-header")