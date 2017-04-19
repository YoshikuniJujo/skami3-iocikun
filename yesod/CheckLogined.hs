module CheckLogined (checkLogined) where

import Import.NoFoundation (
	Maybe(..), Text, ($), (.), (=<<),
	flip, maybe, fromMaybe, isJust, return, mapM_, lookupCookie )
import MyDatabase (
	MyHandler, UserId,
	fromSession, fromAutoLogin, getName, makeSession, updateAutoLogin )

checkLogined :: MyHandler (Text, (Text, Text))
checkLogined = do
	uid <- maybe autoLogin (return . Just) =<< fromSession
	name <- maybe (return Nothing) getName uid
	return (
		fromMaybe "ゲスト" name,
		if isJust uid
			then ("logout", "/ylogout")
			else ("login", "/ylogin") )

autoLogin :: MyHandler (Maybe UserId)
autoLogin = do
	al <- lookupCookie "auto-login"
	uid <- maybe (return Nothing) fromAutoLogin al
	flip (maybe $ return Nothing) uid $ \u -> do
		mapM_ ($ u) [makeSession, updateAutoLogin]
		return $ Just u
