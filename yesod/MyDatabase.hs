{-# LANGUAGE Rank2Types #-}

module MyDatabase (
	MyHandler, UserId(..), AccessToken(..), getRand, rndToTxt, unstring,
	makeSession, makeAutoLogin, updateAutoLogin,
	fromSession, fromAutoLogin, getName ) where

import Import.NoFoundation hiding (
	(==.), (=.), update, delete, UserId
	)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Text as Txt
import qualified Data.HashMap.Lazy as HML
import qualified Data.Aeson as Aeson

import Database.Esqueleto hiding (on) -- (Value(..), (^.), val)
import Web.Cookie (SetCookie(..), sameSiteStrict)

import Crypto.Random
import Network.HTTP.Simple
import Text.Read (readMaybe)

newtype UserId = UserId Text deriving Show
newtype AccessToken = AccessToken Text deriving Show

type MyHandler a = forall site . (
		BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryWrite (YesodPersistBackend site),
		PersistUniqueWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site ) => HandlerT site IO a

makeSession :: UserId -> MyHandler ()
makeSession (UserId uid) = do
	ssn <- lift (getRand 256)
	now <- liftIO getCurrentTime
	_ <- runDB $ do
		delete . from $ \s ->
			where_ $ s ^. SessionUserId ==. val uid
		insert $ Session (rndToTxt ssn) uid now
	setCookie def {
		setCookieName = "session",
		setCookieValue = rndToBs ssn,
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

makeAutoLogin :: UserId -> MyHandler ()
makeAutoLogin (UserId uid) = do
	al <- lift (getRand 512)
	now <- liftIO getCurrentTime
	_ <- runDB $ do
		delete . from $ \s ->
			where_ $ s ^. AutoLoginUserId ==. val uid
		insert $ AutoLogin (rndToTxt al) uid now
	setCookie def {
		setCookieName = "auto-login",
		setCookieValue = rndToBs al,
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

updateAutoLogin :: UserId -> MyHandler ()
updateAutoLogin (UserId uid) = do
	al <- lift (getRand 512)
	_ <- runDB $ do
		update $ \a -> do
			set a [ AutoLoginAutoLogin =. val (rndToTxt al) ]
			where_ (a ^. AutoLoginUserId ==. val uid)
	setCookie def {
		setCookieName = "auto-login",
		setCookieValue = rndToBs al,
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

fromSession :: MyHandler (Maybe UserId)
fromSession = do
	session <- lookupCookie "session"
	us <- flip (maybe $ return []) session $ \ssn ->
		runDB $ select . from $ \s -> do
			where_ $ s ^. SessionSession ==. (val ssn)
			return $ s ^. SessionUserId
	return $ case us of
		[Value u] -> Just $ UserId u
		_ -> Nothing

fromAutoLogin :: Text -> MyHandler (Maybe UserId)
fromAutoLogin al = do
	uid <- runDB $ select . from $ \a -> do
		where_ $ a ^. AutoLoginAutoLogin ==. (val al)
		return $ a ^. AutoLoginUserId
	return $ case uid of
		[Value u] -> Just $ UserId u
		_ -> Nothing

getName :: UserId -> MyHandler (Maybe Text)
getName (UserId u) = do
	names <- runDB . select . from $ \p -> do
			where_ $ p ^. ProfileUserId ==. val u
			return $ p ^. ProfileName
	return $ case names of
		[Value n] -> Just n
		_ -> Nothing

newtype Random = Random { rndToBs :: ByteString }

getRand :: MonadRandom m => Int -> m Random
getRand = (Random . fst . BSC.spanEnd (== '=') . B64.encode <$>)
	. getRandomBytes

rndToTxt :: Random -> Text
rndToTxt = decodeUtf8 . rndToBs

unstring :: Aeson.Value -> Maybe Text
unstring (String t) = Just t
unstring _ = Nothing

{-
getProfile ::
       (MonadIO m, MonadThrow m) => AccessToken -> m (Maybe Aeson.Object)
getProfile (AccessToken at) = do
       initReq <- parseRequest $
               "https://userinfo.yahooapis.jp/yconnect/v1/attribute?" <>
               "schema=openid"
       let     req = setRequestHeader
                       "Authorization" ["Bearer " <> encodeUtf8 at] initReq
       rBody <- getResponseBody <$> httpLBS req
       return (Aeson.decode rBody :: Maybe Aeson.Object)

setProfile :: HashMap Text Aeson.Value -> MyHandler ()
setProfile prf = flip (maybe $ putStrLn eMsg) pu $ \(p, u) -> runDB $ do
       delete . from $ where_ . (==. val u) . (^. ProfileUserId)
       () <$ insert p
       where
       eMsg = "setProfile: error"
       pu = (,) <$> mp <*> uid
       mp = Profile <$> uid <*> n <*> fn <*> gn <*> gd <*> bd <*> em
       [uid, n, fn, gn, gd, bd_, em] = flip lookupString prf <$> [
               "user_id", "name", "family_name", "given_name",
               "gender", "birthday", "email" ]
       bd = readMaybe =<< Txt.unpack <$> bd_
       lookupString k = (unstring =<<) . HML.lookup k
       -}
