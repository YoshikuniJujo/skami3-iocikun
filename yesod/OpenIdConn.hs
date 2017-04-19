{-# LANGUAGE ScopedTypeVariables #-}

module OpenIdConn (
	UserId(..), AccessToken,
	yconnect, authenticate,
	getProfile, setProfile, showProfile,
	makeSession, makeAutoLogin, updateAutoLogin ) where

import Prelude (read)
import Import.NoFoundation hiding (
	UserId, (==.), delete, Header, check, authenticate, lookup, (=.), update )

import Environment

import Crypto.Random

import Control.Arrow (left)

import Data.ByteArray
import Data.Time.Clock.POSIX
import Data.Scientific
import Network.HTTP.Simple
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Database.Esqueleto hiding (on) -- (Value(..), (^.), val)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Text as Txt
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HML

import Web.Cookie (SetCookie(..), sameSiteStrict)

import qualified Data.Text.Encoding as TE

getNonce :: MonadRandom m => Int -> m Text
getNonce = (Txt.dropWhileEnd (== '=') . decodeUtf8 . B64.encode <$>)
	. getRandomBytes

yconnect :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryRead (YesodPersistBackend site),
		PersistStoreWrite (YesodPersistBackend site),
		YesodPersist site) =>
	ClientId -> RedirectUri -> HandlerT site IO b
yconnect cid ruri = do
	(state, nonce, date) <- lift
		$ (,,) <$> getNonce 256 <*> getNonce 256 <*> getCurrentTime
	_ <- runDB $ insert $ OpenIdStateNonce state nonce date
	runDB (selectList ([] :: [Filter OpenIdStateNonce]) [])
		>>= mapM_ print
	redirect $
		"https://auth.login.yahoo.co.jp/yconnect/v1/authorization?" <>
		"response_type=code+id_token&" <>
		"scope=openid+profile+email&" <>
		"client_id=" <> cidToTxt cid <> "&" <>
		"state=" <> state <> "&" <>
		"nonce=" <> nonce <> "&" <>
		"redirect_uri=" <> ruToTxt ruri <> "&" <>
		"bail=1"

lookupGetCode :: MonadHandler f => f (Maybe Code)
lookupGetCode = (Code <$>) <$> lookupGetParam "code"

lookupGetState :: MonadHandler f => f (Maybe State)
lookupGetState = (State <$>) <$> lookupGetParam "state"

newtype State = State Text
newtype Nonce0 = Nonce0 Text
newtype Code = Code Text

codeToBs :: Code -> BS.ByteString
codeToBs (Code c) = encodeUtf8 c

newtype Iss = Iss Text deriving Show
newtype Aud = Aud Text deriving Show
newtype Now = Now POSIXTime deriving Show
newtype Iat = Iat POSIXTime deriving Show
newtype Exp = Exp POSIXTime deriving Show
newtype Nonce1 = Nonce1 Text deriving Show

newtype Signature = Signature Text deriving Show

newtype Header = Header Text deriving Show
newtype Payload = Payload Text deriving Show

headerToAeson :: Maybe Header -> Either String Aeson.Object
headerToAeson (Just (Header t)) = toAeson t
headerToAeson Nothing = Left "Can't get HEADER"

payloadToAeson :: Maybe Payload -> Either String Aeson.Object
payloadToAeson (Just (Payload t)) = toAeson t
payloadToAeson Nothing = Left "Can't get PAYLOAD"

toAeson :: Text -> Either String Aeson.Object
toAeson t = do
	b <- left ("B64.decode error: " ++)
		. B64.decode . encodeUtf8 $ padding t
	maybe (Left "Aeson.decode error") Right
		. Aeson.decode $ LBS.fromStrict b

newtype TokenType = TokenType Text deriving (Show, Eq)

newtype UserId = UserId Text deriving Show
newtype AccessToken = AccessToken Text deriving Show

-- authenticate, logined :: Handler (Either String (UserId, AccessToken))
-- authenticate, logined :: HandlerT App IO (Either String (UserId, AccessToken))
authenticate, logined :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistUniqueWrite (YesodPersistBackend site),
		PersistQueryWrite (YesodPersistBackend site),
		YesodPersist site,
		IsPersistBackend (YesodPersistBackend site)) =>
	HandlerT site IO (Either String (UserId, AccessToken))
authenticate = logined

logined = do
	cs <- (\c s -> (,) <$> c <*> s) <$> lookupGetCode <*> lookupGetState
	maybe (return $ Left "no code or state") (uncurry logined_) cs

-- logined_ ::
--	Code -> State -> HandlerT App IO (Either String (UserId, AccessToken))
logined_ :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistUniqueWrite (YesodPersistBackend site),
		PersistQueryWrite (YesodPersistBackend site),
		YesodPersist site,
		IsPersistBackend (YesodPersistBackend site)) =>
	Code -> State -> HandlerT site IO (Either String (UserId, AccessToken))
logined_ code (State state) = do
	nonce <- runDB $ do
		n <- select . from $ \sn -> do
			where_ $ sn ^. OpenIdStateNonceState ==. val state
			return ( sn ^. OpenIdStateNonceNonce )
		delete . from $ \sc -> do
			where_ $ sc ^. OpenIdStateNonceState ==. val state
		return n
	case nonce of
		[Value n] -> loginedGen code (Nonce0 n)
		_ -> return $ Left "No or Multiple state"

loginedGen :: (MonadIO (t IO), MonadThrow (t IO), MonadTrans t) =>
	Code -> Nonce0 -> t IO (Either String (UserId, AccessToken))
loginedGen code n0 = do
	(clientId, clientSecret, redirectUri) <- lift $ (,,) 
		<$> getClientId <*> getClientSecret <*> getRedirectUri
	initReq <-
		parseRequest "https://auth.login.yahoo.co.jp/yconnect/v1/token"
	let	req = foldr (uncurry setRequestHeader)
			initReq { method = "POST" } [
			("Content-Type", ["application/x-www-form-urlencoded"]),
			basicAuthentication clientId clientSecret ]
		req' = (`setRequestBody` req) . RequestBodyBS $
			"grant_type=authorization_code&" <>
			"code=" <> codeToBs code <> "&" <>
			"redirect_uri=" <> ruToBs redirectUri
	resp :: Maybe Aeson.Object <-
		Aeson.decode . getResponseBody <$> httpLBS req'
	let	ei = HML.lookup "expires_in" =<< resp
		rt = HML.lookup "refresh_token" =<< resp
	putStr "expires_in: "; print ei
	putStr "refresh_token: "; print rt
	let	tt = TokenType
			<$> (unstring =<< HML.lookup "token_type" =<< resp)
	now <- lift getPOSIXTime
	return $ something tt clientId clientSecret n0 (Now now) =<<
		maybe (Left "Can't decode to Aeson.Object") Right resp

something :: Maybe TokenType -> ClientId -> ClientSecret -> Nonce0 -> Now ->
	HashMap Text Aeson.Value -> (Either String (UserId, AccessToken))
something tt clientId clientSecret n0 now resp = do
	let
		mat = AccessToken
			<$> (unstring =<< HML.lookup "access_token" resp)
		mit = unstring =<< HML.lookup "id_token" resp
		(mhd, mpl, msg) = case Txt.splitOn "." <$> mit of
			Just [h, p, s] -> (
				Just $ Header h,
				Just $ Payload p,
				Just $ Signature s)
			_ -> (Nothing, Nothing, Nothing)
	pld <- payloadToAeson mpl
	let
		miss = Iss <$> (unstring =<< HML.lookup "iss" pld)
		maud = Aud <$> (unstring =<< HML.lookup "aud" pld)
		miat = Iat . fromRational . toRational
			<$> (unnumber =<< HML.lookup "iat" pld)
		mex = Exp . fromRational . toRational
			<$> (unnumber =<< HML.lookup "exp" pld)
		mn1 = Nonce1 <$> (unstring =<< HML.lookup "nonce" pld)
		muid = UserId <$> (unstring =<< HML.lookup "user_id" pld)
	hdd <- headerToAeson mhd
	when (tt /= Just (TokenType "bearer")) $ Left "BAD TOKEN_TYPE"
	when (hdd /= HML.fromList [
		("alg", String "HS256"),
		("typ", String "JWT") ]) $ Left "BAD HEADER"
	check miss (maud, clientId) (miat, mex, now) (mn1, n0)
		(clientSecret, mhd, mpl, msg) (muid, mat)

type M = Maybe

check :: M Iss -> (M Aud, ClientId) -> (M Iat, M Exp, Now) ->
	(M Nonce1, Nonce0) ->
	(ClientSecret, M Header, M Payload, M Signature) ->
	(M UserId, M AccessToken) ->
	Either String (UserId, AccessToken)
check miss (maud, cid) (miat, mex, now) (mn1, n0) (cs, mhd, mpl, msg)
	(muid, mat) = do
	(iss, aud, iat, ex, n1, hd, pl, sg, uid, at) <-
		existence miss maud miat mex mn1 mhd mpl msg muid mat
	checkGen iss (aud, cid)
		(iat, ex, now) (n1, n0) (cs, hd, pl, sg) (uid, at)

existence :: M Iss -> M Aud -> M Iat -> M Exp -> M Nonce1 -> M Header ->
	M Payload -> M Signature -> M UserId -> M AccessToken ->
	Either String (
		Iss, Aud, Iat, Exp, Nonce1, Header, Payload, Signature,
		UserId, AccessToken )
existence miss maud miat mexp mn1 mhd mpl msg mui mat = do
	iss <- maybe (Left "NO ISS") Right miss
	aud <- maybe (Left "NO AUD") Right maud
	iat <- maybe (Left "NO IAT") Right miat
	ex <- maybe (Left "NO EXP") Right mexp
	n1 <- maybe (Left "NO NONCE") Right mn1
	hd <- maybe (Left "NO HEADER") Right mhd
	pl <- maybe (Left "NO PAYLOAD") Right mpl
	sg <- maybe (Left "NO SIGNATURE") Right msg
	ui <- maybe (Left "NO USERID") Right mui
	at <- maybe (Left "NO ACCESSTOKEN") Right mat
	return (iss, aud, iat, ex, n1, hd, pl, sg, ui, at)

checkGen :: Iss -> (Aud, ClientId) -> (Iat, Exp, Now) -> (Nonce1, Nonce0) ->
	(ClientSecret, Header, Payload, Signature) ->
	(UserId, AccessToken) ->
	Either String (UserId, AccessToken)
checkGen (Iss iss) (Aud aud, cid) (Iat iat, Exp ex, Now now)
	(Nonce1 n1, Nonce0 n0)
	(cs, Header hd, Payload pl, Signature sg)
	(uid, at) = do
	when (iss /= "https://auth.login.yahoo.co.jp") $ Left "BAD ISS"
	when (aud /= cidToTxt cid) $ Left "BAD AUD"
	when (iat < now - 600) $ Left "BAD IAT"
	when (ex < now) $ Left "BAD EXP"
	when (n1 /= n0) $ Left "BAD NONCE"
	let sg1 = decodeUtf8
		. fst . BSC.spanEnd (== '=') . hmacSha256 (csToBs cs)
		$ encodeUtf8 hd <> "." <> encodeUtf8 pl
	when (sg1 /= sg) $ Left "BAD SIGNATURE"
	return (uid, at)

basicAuthentication ::
	IsString s => ClientId -> ClientSecret -> (s, [ByteString])
basicAuthentication cid cs = ("Authorization", ["Basic " <> mkClientIdSecret])
	where mkClientIdSecret = B64.encode $ cidToBs cid <> ":" <> csToBs cs

unstring :: Aeson.Value -> Maybe Text
unstring (String t) = Just t
unstring _ = Nothing

unnumber :: Aeson.Value -> Maybe Scientific
unnumber (Number n) = Just n
unnumber _ = Nothing

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 s d = B64.encode . convert $ hmacGetDigest (hmac s d :: HMAC SHA256)

padding :: Text -> Text
padding t = t <> Txt.replicate (3 - (Txt.length t - 1) `mod` 4) "="

showProfile :: HashMap Text Aeson.Value -> [Text]
showProfile json =
	map showSimple . sortBy (compare `on` fst) . HML.toList $ json
	where
	showSimple :: (Text, Aeson.Value) -> Text
	showSimple (k, String v) = k <> ": " <> v
	showSimple (k, v) = k <> ": " <> Txt.pack (show v)

getProfile ::
	(MonadIO m, MonadThrow m) => AccessToken -> m (Maybe Aeson.Object)
getProfile (AccessToken at) = do
	initReq <- parseRequest $
		"https://userinfo.yahooapis.jp/yconnect/v1/attribute?" <>
		"schema=openid"
	let	req = setRequestHeader
			"Authorization" ["Bearer " <> encodeUtf8 at] initReq
	rBody <- getResponseBody <$> httpLBS req
	return (Aeson.decode rBody :: Maybe Aeson.Object)

lookupString :: (Hashable k, Eq k) => k -> HashMap k Aeson.Value -> Maybe Text
lookupString k = (unstring =<<) . HML.lookup k

makeSession :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistUniqueWrite (YesodPersistBackend site),
		PersistQueryWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site) =>
	UserId -> HandlerT site IO ()
makeSession (UserId uid) = do
	ssn <- lift (getNonce 256)
	now <- liftIO getCurrentTime
	_ <- runDB $ do
		delete . from $ \s ->
			where_ $ s ^. SessionUserId ==. val uid
		insert $ Session ssn uid now
	setCookie def {
		setCookieName = "session",
		setCookieValue = encodeUtf8 ssn,
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

makeAutoLogin :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistUniqueWrite (YesodPersistBackend site),
		PersistQueryWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site) =>
	UserId -> HandlerT site IO ()
makeAutoLogin (UserId uid) = do
	al <- lift (getNonce 512)
	now <- liftIO getCurrentTime
	_ <- runDB $ do
		delete . from $ \s ->
			where_ $ s ^. AutoLoginUserId ==. val uid
		insert $ AutoLogin al uid now
	setCookie def {
		setCookieName = "auto-login",
		setCookieValue = encodeUtf8 al,
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

updateAutoLogin :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistUniqueWrite (YesodPersistBackend site),
		PersistQueryWrite (YesodPersistBackend site),
		YesodPersist site,
		IsPersistBackend (YesodPersistBackend site)) =>
	UserId -> HandlerT site IO ()
updateAutoLogin (UserId uid) = do
	al <- lift (getNonce 512)
	_ <- runDB $ do
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

setProfile :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistUniqueWrite (YesodPersistBackend site),
		PersistQueryWrite (YesodPersistBackend site),
		YesodPersist site,
		IsPersistBackend (YesodPersistBackend site)) =>
	HashMap Text Aeson.Value -> HandlerT site IO ()
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
	bd = read . Txt.unpack <$> bd_
