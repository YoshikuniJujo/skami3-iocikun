module Handler.Home where

import Import (
	Handler, Html,
	($), (>>=), (=<<), (<$>), (<*>),
	Maybe(..), uncurry, lift, print,
	getClientId, getRedirectUri,
	widgetFile, defaultLayout, setTitle,
	lookupSession, setSession,
	lookupCookie, setCookie, def,
	Route(..))

import Web.Cookie (SetCookie(..))

getHomeR :: Handler Html
getHomeR = do
	lookupSession "hoge" >>= print
	setSession "hoge" "hige"
	lookupCookie "hoge" >>= print
	setCookie def {
		setCookieName = "hoge",
		setCookieValue = "hidebu",
		setCookieMaxAge = Just 10 }
	defaultLayout $ do
		setTitle "Welcome To Skami3!"
		$(widgetFile "homepage")
