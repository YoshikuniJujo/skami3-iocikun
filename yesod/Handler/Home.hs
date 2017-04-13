module Handler.Home where

import Import (
	Handler, Html,
	($), (=<<), (<$>), (<*>),
	uncurry, lift, getClientId, getRedirectUri,
	widgetFile, defaultLayout, setTitle,
	Route(..))
import OpenIdCon (yconnect)

getHomeR :: Handler Html
getHomeR = do
	defaultLayout $ do
		setTitle "Welcome To Skami3!"
		$(widgetFile "homepage")
