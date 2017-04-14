module Handler.Home where

import Import (
	Handler, Html,
	($), (.),
	flip, maybe, print, return,
	widgetFile, defaultLayout, setTitle,
	lookupCookie, runDB, null,
	)

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
