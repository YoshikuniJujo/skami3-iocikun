module OpenIdCon (
	yconnect,
	Code(..),
	State(..),
	UserId(..),
	AccessToken(..),
	debugProfile,
	logined
	) where

import Import hiding (UserId, (==.), delete, Header, check)

import Data.Maybe (fromJust)
import Data.ByteArray
import Data.Time.Clock.POSIX
import Data.Scientific
import Network.HTTP.Simple
import Crypto.Random
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Database.Esqueleto hiding (on) -- (Value(..), (^.), val)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Text as Txt
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HML

getNonce :: MonadRandom m => Int -> m Text
getNonce = (Txt.dropWhileEnd (== '=') . decodeUtf8 . B64.encode <$>)
	. getRandomBytes

yconnect :: ClientId -> RedirectUri -> Handler Html
yconnect (ClientId cid) (RedirectUri ruri) = do
	(state, nonce, date) <- lift
		$ (,,) <$> getNonce 256 <*> getNonce 256 <*> getCurrentTime
	_ <- runDB $ insert $ OpenIdStateNonce state nonce date
	runDB (selectList ([] :: [Filter OpenIdStateNonce]) [])
		>>= mapM_ print
	yc state nonce
	where
	yc :: MonadHandler m => Text -> Text -> m a
	yc stt nnc = redirect $
		"https://auth.login.yahoo.co.jp/yconnect/v1/authorization?" <>
		"response_type=code+id_token&" <>
		"scope=openid+profile+email&" <>
		"client_id=" <> cid <> "&" <>
		"state=" <> stt <> "&" <>
		"nonce=" <> nnc <> "&" <>
		"redirect_uri=" <> ruri <> "&" <>
		"bail=1"

newtype Code = Code Text
newtype State = State Text
newtype Nonce0 = Nonce0 Text

newtype Iss = Iss Text deriving Show
newtype Aud = Aud Text deriving Show
newtype Now = Now POSIXTime deriving Show
newtype Iat = Iat POSIXTime deriving Show
newtype Exp = Exp POSIXTime deriving Show
newtype Nonce1 = Nonce1 Text deriving Show

newtype Signature = Signature Text deriving Show
newtype Header = Header Text deriving Show
newtype Payload = Payload Text deriving Show

newtype UserId = UserId Text deriving Show
newtype AccessToken = AccessToken Text deriving Show

logined :: Code -> State -> Handler (Either String (UserId, AccessToken))
logined code (State state) = do
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

loginedGen :: Code -> Nonce0 -> Handler (Either String (UserId, AccessToken))
loginedGen (Code code) (Nonce0 n0) = do
	(clientId, clientSecret, redirectUri) <- lift $ (,,) 
		<$> getClientId <*> getClientSecret <*> getRedirectUri
	initReq <-
		parseRequest "https://auth.login.yahoo.co.jp/yconnect/v1/token"
	let	req = foldr (uncurry setRequestHeader)
			initReq { method = "POST" } [
			("Content-Type", ["application/x-www-form-urlencoded"]),
			basicAuthentication clientId clientSecret ]
		req' = setRequestBody (RequestBodyBS $
			"grant_type=authorization_code&" <>
			"code=" <> encodeUtf8 code <> "&" <>
			"redirect_uri=" <> ruToBs redirectUri) req
	rBody <- getResponseBody <$> httpLBS req'

	let	Just resp = Aeson.decode rBody :: Maybe Aeson.Object
		mat = AccessToken
			<$> (unstring =<< HML.lookup "access_token" resp)
		Just (String it) = HML.lookup "id_token" resp
		Just (String ei) = HML.lookup "expires_in" resp
		Just (String tt) = HML.lookup "token_type" resp
		Just (String rt) = HML.lookup "refresh_token" resp
		(mhd_, mpl_, msg_) = case Txt.splitOn "." it of
			[h, p, s] -> (Just h, Just p, Just s)
			_ -> (Nothing, Nothing, Nothing)
	print $ keys resp
	print ei
	print tt
	print rt
	let	[Just hdd, Just pld] = map
			((Aeson.decode :: LBS.ByteString -> Maybe Aeson.Object)
				. LBS.fromStrict
				. either (error . ("B64.decode error " ++) . show) id
				. B64.decode . encodeUtf8)
			[padding $ fromJust mhd_, padding $ fromJust mpl_]
		miss = Iss <$> (unstring =<< HML.lookup "iss" pld)
		maud = Aud <$> (unstring =<< HML.lookup "aud" pld)
		miat = Iat . fromRational . toRational
			<$> (unnumber =<< HML.lookup "iat" pld)
		mex = Exp . fromRational . toRational
			<$> (unnumber =<< HML.lookup "exp" pld)
		mn1 = Nonce1 <$> (unstring =<< lookup "nonce" pld)
		mhd = Header <$> mhd_
		mpl = Payload <$> mpl_
		msg = Signature <$> msg_
		muid = UserId <$> (unstring =<< HML.lookup "user_id" pld)
	now <- lift getPOSIXTime
	print hdd
	print pld
	print muid
	print miss
	print maud
	print clientId
	print miat
	print mex
	print now
	return $ check miss (maud, clientId)
			(miat, mex, Now now) (mn1, Nonce0 n0)
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
checkGen (Iss iss) (Aud aud, ClientId cid) (Iat iat, Exp ex, Now now)
	(Nonce1 n1, Nonce0 n0)
	(ClientSecret cs, Header hd, Payload pl, Signature sg)
	(uid, at) = do
	when (iss /= "https://auth.login.yahoo.co.jp") $ Left "BAD ISS"
	when (aud /= cid) $ Left "BAD AUD"
	when (iat < now - 600) $ Left "BAD IAT"
	when (ex < now) $ Left "BAD EXP"
	when (n1 /= n0) $ Left "BAD NONCE"
	let sg1 = decodeUtf8
		. fst . BSC.spanEnd (== '=') . hmacSha256 (encodeUtf8 cs)
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

debugProfile :: AccessToken -> Handler ()
debugProfile (AccessToken at) = do
	initReq <- parseRequest $
		"https://userinfo.yahooapis.jp/yconnect/v1/attribute?schema=openid"
	let	req = setRequestHeader
			"Authorization" ["Bearer " <> encodeUtf8 at] initReq
	rBody <- getResponseBody <$> httpLBS req
	let	Just json = Aeson.decode rBody :: Maybe Aeson.Object
	mapM_ putStrLn . map showSimple . sortBy (compare `on` fst) $ HML.toList json
	where
	showSimple :: (Text, Aeson.Value) -> Text
	showSimple (k, String v) = k <> ": " <> v
	showSimple (k, v) = k <> ": " <> Txt.pack (show v)
