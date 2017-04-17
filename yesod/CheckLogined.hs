module CheckLogined (checkLogined) where

import Import.NoFoundation hiding ((.), print, (==.), (=.), update, delete)

import Prelude
import Data.Text (Text)
import Database.Esqueleto
import Web.Cookie

import Crypto.Random

import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64.URL as B64

import Data.Pool

checkLogined :: (MonadTrans t, MonadBaseControl IO (t IO),
		MonadHandler (t IO)) =>
	Pool SqlBackend -> t IO (Text, Text, Text)
checkLogined connPool = do
	session <- lookupCookie "session"
	autoLogin <- lookupCookie "auto-login"

	uid1 <- flip (maybe $ return []) session $ \ssn ->
		runDataBase $ select . from $ \s -> do
			where_ $ s ^. SessionSession ==. (val ssn)
			return $ s ^. SessionUserId

	mUserId <- case uid1 of
		[Value u] -> return $ Just u
		_ -> do	uid2 <- flip (maybe $ return []) autoLogin $ \al ->
				runDataBase $ select . from $ \a -> do
					where_ $ a ^. AutoLoginAutoLogin ==.
						(val al)
					return $ a ^. AutoLoginUserId
			case uid2 of
				[Value u'] -> do
					makeSession connPool u'
					updateAutoLogin connPool u'
					return $ Just u'
				_ -> return Nothing
	names <- flip (maybe $ return []) mUserId $ \u ->
		runDataBase . select . from $ \p -> do
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
	where
	runDataBase :: MonadBaseControl IO m => ReaderT SqlBackend m a -> m a
	runDataBase = flip runSqlPool connPool

makeSession :: (MonadHandler (t m), MonadBaseControl IO (t m),
		MonadRandom m, MonadTrans t) =>
	Pool SqlBackend -> Text -> t m ()
makeSession connPool uid = do
	ssn <- lift (getNonce 256)
	now <- liftIO getCurrentTime
	_ <- runDataBase $ do
		delete . from $ \s ->
			where_ $ s ^. SessionUserId ==. val uid
		insert $ Session ssn uid now
	setCookie def {
		setCookieName = "session",
		setCookieValue = TE.encodeUtf8 ssn,
		setCookiePath = Just "/",
		setCookieExpires = Nothing,
		setCookieMaxAge = Just 1800,
		setCookieDomain = Nothing,
		setCookieHttpOnly = True,
#ifdef DEVELOPMENT
		setCookieSecure = False,
#else
		setCookieSecure = True,
#endif
		setCookieSameSite = Just sameSiteStrict
		}
	where
	runDataBase :: MonadBaseControl IO m => ReaderT SqlBackend m a -> m a
	runDataBase = flip runSqlPool connPool

updateAutoLogin :: (MonadHandler (t m), MonadTrans t,
		MonadRandom m, MonadBaseControl IO (t m)) =>
	Pool SqlBackend -> Text -> t m ()
updateAutoLogin connPool uid = do
	al <- lift (getNonce 512)
	_ <- runDataBase $ do
		update $ \a -> do
			set a [ AutoLoginAutoLogin =. val al ]
			where_ (a ^. AutoLoginUserId ==. val uid)
	setCookie def {
		setCookieName = "auto-login",
		setCookieValue = TE.encodeUtf8 al,
		setCookiePath = Just "/",
		setCookieExpires = Nothing,
		setCookieMaxAge = Just 2592000,
		setCookieDomain = Nothing,
		setCookieHttpOnly = True,
#ifdef DEVELOPMENT
		setCookieSecure = False,
#else
		setCookieSecure = True,
#endif
		setCookieSameSite = Just sameSiteStrict
		}
	where
	runDataBase :: MonadBaseControl IO m => ReaderT SqlBackend m a -> m a
	runDataBase = flip runSqlPool connPool

getNonce :: MonadRandom m => Int -> m Text
getNonce = (Txt.dropWhileEnd (== '=') . TE.decodeUtf8 . B64.encode <$>)
	. getRandomBytes
