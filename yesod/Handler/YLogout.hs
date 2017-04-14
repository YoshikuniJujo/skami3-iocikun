module Handler.YLogout (getYLogoutR) where

import Import

getYLogoutR :: Handler Html
getYLogoutR = defaultLayout $ do
	setTitle "Welcome To Skami3!"
	$(widgetFile "ylogout")
