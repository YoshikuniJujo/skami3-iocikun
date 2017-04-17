module CheckLogined (checkLogined) where

import Import.NoFoundation hiding ((.), print, (==.), (=.), update, delete, UserId)

import Prelude
import Database.Esqueleto

import OpenIdCon

checkLogined :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryWrite (YesodPersistBackend site),
		PersistUniqueWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site) =>
	HandlerT site IO (Text, Text, Text)
checkLogined = do
	session <- lookupCookie "session"
	autoLogin <- lookupCookie "auto-login"

	uid1 <- flip (maybe $ return []) session $ \ssn ->
		runDB $ select . from $ \s -> do
			where_ $ s ^. SessionSession ==. (val ssn)
			return $ s ^. SessionUserId

	mUserId <- case uid1 of
		[Value u] -> return $ Just u
		_ -> do	uid2 <- flip (maybe $ return []) autoLogin $ \al ->
				runDB $ select . from $ \a -> do
					where_ $ a ^. AutoLoginAutoLogin ==.
						(val al)
					return $ a ^. AutoLoginUserId
			case uid2 of
				[Value u'] -> do
					makeSession (UserId u')
					updateAutoLogin (UserId u')
					return $ Just u'
				_ -> return Nothing
	names <- flip (maybe $ return []) mUserId $ \u ->
		runDB . select . from $ \p -> do
			where_ $ p ^. ProfileUserId ==. val u
			return $ p ^. ProfileName
	lift $ print names
	let	(loginOut, loginOutLn) = case mUserId of
			Just _ -> ("logout" :: Text, "/ylogout" :: Text)
			Nothing -> ("login", "/ylogin")
		userName = case names of
			[Value n] -> n
			_ -> "ゲスト"
	return (userName, loginOut, loginOutLn)
