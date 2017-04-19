module CheckLogined (checkLogined) where

import Import.NoFoundation (
	Maybe(..), Text, IO, HandlerT, YesodPersist, YesodPersistBackend,
	return, ($), (.), runDB, maybe, flip, lookupCookie,
	IsPersistBackend, PersistUniqueWrite, PersistQueryWrite,
	SqlBackend, BaseBackend, fromMaybe, isJust, (=<<))
import Model (
	EntityField(ProfileUserId, ProfileName, AutoLoginUserId, AutoLoginAutoLogin, SessionUserId, SessionSession))

import Database.Esqueleto (
	Value(..), (^.), val, (==.), where_, from, select)

import OpenIdConn (UserId(..), updateAutoLogin, makeSession)

checkLogined :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryWrite (YesodPersistBackend site),
		PersistUniqueWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site) =>
	HandlerT site IO (Text, (Text, Text))
checkLogined = do
	uid <- maybe fromAutoLogin (return . Just) =<< fromSession
	name <- maybe (return Nothing) getName uid
	return (
		fromMaybe "ゲスト" name,
		if isJust uid
			then ("logout", "/ylogout")
			else ("login", "/ylogin") )

fromSession :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryWrite (YesodPersistBackend site),
		PersistUniqueWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site) =>
	HandlerT site IO (Maybe UserId)
fromSession = do
	session <- lookupCookie "session"
	us <- flip (maybe $ return []) session $ \ssn ->
		runDB $ select . from $ \s -> do
			where_ $ s ^. SessionSession ==. (val ssn)
			return $ s ^. SessionUserId
	return $ case us of
		[Value u] -> Just $ UserId u
		_ -> Nothing

fromAutoLogin :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryWrite (YesodPersistBackend site),
		PersistUniqueWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site) =>
	HandlerT site IO (Maybe UserId)
fromAutoLogin = do
	autoLogin <- lookupCookie "auto-login"
	uid2 <- flip (maybe $ return []) autoLogin $ \al ->
		runDB $ select . from $ \a -> do
			where_ $ a ^. AutoLoginAutoLogin ==. (val al)
			return $ a ^. AutoLoginUserId
	case uid2 of
		[Value u'] -> do
			makeSession (UserId u')
			updateAutoLogin (UserId u')
			return . Just $ UserId u'
		_ -> return Nothing

getName :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryWrite (YesodPersistBackend site),
		PersistUniqueWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site) =>
	UserId -> HandlerT site IO (Maybe Text)
getName (UserId u) = do
	names <- runDB . select . from $ \p -> do
			where_ $ p ^. ProfileUserId ==. val u
			return $ p ^. ProfileName
	return $ case names of
		[Value n] -> Just n
		_ -> Nothing
