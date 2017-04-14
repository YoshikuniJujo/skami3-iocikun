module Handler.Home where

import Import (
	Handler, Html,
	($), (.), (>>=), (=<<), (<$>), (<*>),
	Bool(..), Maybe(..), uncurry, lift, show, print,
	getClientId, getRedirectUri,
	widgetFile, defaultLayout, setTitle,
	lookupSession, setSession,
	lookupCookie, setCookie, def,
	Route(..))

import Web.Cookie (SetCookie(..), sameSiteStrict)

import qualified Data.Text as Txt

getHomeR :: Handler Html
getHomeR = do
	lookupSession "hoge" >>= print
	setSession "hoge" "hige"
	hoge <- show <$> lookupCookie "hoge"
	print hoge
	session <- (Txt.take 15 <$>) <$> lookupCookie "session"
	print session
	setCookie def {
		setCookieName = "hoge",
		setCookieValue = "hidebu",
		setCookiePath = Just "/",
		setCookieExpires = Nothing,
		setCookieMaxAge = Just 10,
		setCookieDomain = Nothing,
		setCookieHttpOnly = True,
#ifdef DEVELOPMENT
		setCookieSecure = False,
#else
		setCookieSecure = True,
#endif
		setCookieSameSite = Just sameSiteStrict
		}
	defaultLayout $ do
		setTitle "Welcome To Skami3!"
		$(widgetFile "homepage")
