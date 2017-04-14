module Handler.Home where

import Import (
	Handler, Html,
	($), (.), (>>=), (=<<), (<$>), (<*>),
	Bool(..), Maybe(..), uncurry, flip, maybe, lift, show, print, return,
	getClientId, getRedirectUri,
	widgetFile, defaultLayout, setTitle,
	lookupSession, setSession,
	lookupCookie, setCookie, def, runDB, hamlet, null,
	Route(..) )

import Web.Cookie (SetCookie(..), sameSiteStrict)

import qualified Data.Text as Txt

import Model
import Database.Esqueleto

getHomeR :: Handler Html
getHomeR = do
	session <- lookupCookie "session"
	print session
	uid <- flip (maybe $ return []) session $ \ssn -> runDB
		. select . from $ \s -> do
			where_ $ s ^. SessionSession ==. val ssn
			return $ s ^. SessionUserId
	print uid

	autoLogin <- lookupCookie "auto-login"
	uid2 <- case uid of
		[_] -> return uid
		_ -> flip (maybe $ return []) autoLogin $ \al -> runDB
			. select . from $ \a -> do
				where_ $ a ^. AutoLoginAutoLogin ==. val al
				return $ a ^. AutoLoginUserId
	print uid2
	defaultLayout $ do
		setTitle "Welcome To Skami3!"
		$(widgetFile "homepage")
