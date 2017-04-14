module Handler.YLogout (getYLogoutR) where

import Import hiding ((==.), delete)

import Database.Esqueleto

getYLogoutR :: Handler Html
getYLogoutR = do
	session <- lookupCookie "session"
	autoLogin <- lookupCookie "auto-login"
	flip (maybe $ return ()) session $ \ssn -> runDB $ do
		delete . from $ \s -> do
			where_ $ s ^. SessionSession ==. val ssn
	flip (maybe $ return ()) autoLogin $ \al -> runDB $ do
		delete . from $ \a -> do
			where_ $ a ^. AutoLoginAutoLogin ==. val al
	redirect ("/" :: Text)
			{-
	defaultLayout $ do
		setTitle "Welcome To Skami3!"
		$(widgetFile "ylogout")
		-}
