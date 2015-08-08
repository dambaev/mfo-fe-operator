module Widgets.MainMenu
    ( mainMenuW
    )
    where

import Import

import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

mainMenuW:: Widget 
mainMenuW = do
    menuMainDivI <- newIdent
    navI <- newIdent
    $(widgetFile "mainmenu/mainmenu")