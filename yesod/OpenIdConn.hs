{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

module OpenIdConn (
	UserId, AccessToken,
	yconnect, authenticate, getProfile, setProfile ) where

import Control.Arrow (left)
import Data.ByteArray (convert)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, getCurrentTime)
import Text.Read (readMaybe)
import Network.HTTP.Simple (
	httpLBS, getResponseBody,
	parseRequest, setRequestBody, setRequestHeader )
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Database.Esqueleto (
	Value(..), (^.), (==.), select, insert, delete, from, where_, val )

import Import.NoFoundation (
	Eq, Show, MonadIO, MonadThrow,
	Maybe(..), Either(..), Text, ByteString, String, HashMap,
	Profile(..), Value(..), RequestBody(..), HeaderName,
	($), (.), (<$>), (<$), (<*>), (=<<),
	(/=), (==), (<), (<>), (++), (-),
	flip, maybe, either, uncurry, fst, foldr, return, lift, when,
	putStrLn, toRational, fromRational, mod,
	encodeUtf8, decodeUtf8, method, runDB, redirect, lookupGetParam )
import Model (
	EntityField(
		OpenIdStateNonceState, OpenIdStateNonceNonce,
		ProfileUserId ),
	OpenIdStateNonce(..) )
import Environment (
	ClientId, ClientSecret, RedirectUri,
	cidToTxt, cidToBs, csToBs, ruToTxt, ruToBs )
import Common (
	MyHandler, UserId(..), AccessToken(..),
	getRand, rndToTxt, unstring, unnumber )

import qualified Data.Text as Txt
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.HashMap.Lazy as HML
import qualified Data.Aeson as Aeson

yconnect :: ClientId -> RedirectUri -> MyHandler a
yconnect cid ruri = do
	(state, nonce, date) <- lift
		$ (,,) <$> getRand 256 <*> getRand 256 <*> getCurrentTime
	_ <- runDB . insert
		$ OpenIdStateNonce (rndToTxt state) (rndToTxt nonce) date
	redirect $
		"https://auth.login.yahoo.co.jp/yconnect/v1/authorization?" <>
		"response_type=code+id_token&" <>
		"scope=openid+profile+email&" <>
		"client_id=" <> cidToTxt cid <> "&" <>
		"state=" <> rndToTxt state <> "&" <>
		"nonce=" <> rndToTxt nonce <> "&" <>
		"redirect_uri=" <> ruToTxt ruri <> "&" <>
		"bail=1"

authenticate ::
	ClientId -> ClientSecret -> RedirectUri ->
	MyHandler (Either String (UserId, AccessToken))
authenticate cid csc ruri = do
	cs <- (\c s -> (,) <$> c <*> s) <$> lookupGetCode <*> lookupGetState
	flip (maybe . return $ Left "no code or state") cs $ \(c, s) -> do
		en <- getNonceFromState s
		flip (either $ return . Left) en . (lift .) $ \n0 -> do
			uaTbc <- requestUserId cid csc ruri c
			now <- getPOSIXTime
			return $ do
				(ua, tbc) <- uaTbc
				checkAll cid csc n0 now tbc
				return ua

requestUserId :: (MonadIO m, MonadThrow m) =>
	ClientId -> ClientSecret -> RedirectUri -> Code ->
	m (Either String ((UserId, AccessToken), ToBeChecked))
requestUserId cid cs ruri code = do
	initReq <-
		parseRequest "https://auth.login.yahoo.co.jp/yconnect/v1/token"
	let	req = foldr (uncurry setRequestHeader)
			initReq { method = "POST" } [
			("Content-Type", ["application/x-www-form-urlencoded"]),
			basicAuthentication cid cs ]
		req' = (`setRequestBody` req) . RequestBodyBS $
			"grant_type=authorization_code&" <>
			"code=" <> codeToBs code <> "&" <>
			"redirect_uri=" <> ruToBs ruri
	resp :: Maybe Aeson.Object <-
		Aeson.decode . getResponseBody <$> httpLBS req'
	let	ett = TokenType <$> maybe (Left "NO TOKEN_TYPE") Right
			(unstring =<< HML.lookup "token_type" =<< resp)
		eat = AccessToken <$> maybe (Left "NO ACCESS_TOKEN") Right
			(unstring =<< HML.lookup "access_token" =<< resp)
		eit = maybe (Left "NO ID_TOKEN") Right
			$ unstring =<< HML.lookup "id_token" =<< resp
	return $ do
		tt <- ett
		it <- eit
		at <- eat
		(uid, tbc) <- getUidAndTbc tt it
		return ((uid, at), tbc)

basicAuthentication :: ClientId -> ClientSecret -> (HeaderName, [ByteString])
basicAuthentication cid' cs' = (
	"Authorization",
	["Basic " <> B64.encode (cidToBs cid' <> ":" <> csToBs cs')] )

getUidAndTbc :: TokenType -> Text -> Either String (UserId, ToBeChecked)
getUidAndTbc tt it = do
	(hd, pl, sg) <- case Txt.splitOn "." it of
		[h, p, s] -> return (h, p, s)
		_ -> Left "BAD ID_TOKEN"
	pld <- payloadToAeson (Payload pl)
	uid <- UserId <$> maybe (Left "NO USER_ID") Right
		(unstring =<< HML.lookup "user_id" pld)
	iss <- Iss <$> maybe (Left "NO ISS") Right
		(unstring =<< HML.lookup "iss" pld)
	aud <- Aud <$> maybe (Left "NO AUD") Right
		(unstring =<< HML.lookup "aud" pld)
	iat <- Iat . fromRational . toRational
		<$> maybe (Left "NO IAT") Right
			(unnumber =<< HML.lookup "iat" pld)
	ex <- Exp . fromRational . toRational
		<$> maybe (Left "NO EXP") Right
			(unnumber =<< HML.lookup "exp" pld)
	n1 <- Nonce1 <$> maybe (Left "NO Nonce") Right
		(unstring =<< HML.lookup "nonce" pld)
	return (uid, ToBeChecked {
		tbcTokenType = tt,
		tbcIss = iss,
		tbcAud = aud,
		tbcIat = iat,
		tbcExp = ex,
		tbcNonce = n1,
		tbcHeader = Header hd,
		tbcPayload = Payload pl,
		tbcSignature = Signature sg } )

checkAll ::
	ClientId -> ClientSecret -> Nonce0 -> POSIXTime -> ToBeChecked ->
	Either String ()
checkAll cid cs (Nonce0 n0) now tbc = do
	when (tbcTokenType tbc /= TokenType "bearer") $
		Left "BAD TOKEN_TYPE"
	hdd <- headerToAeson $ tbcHeader tbc
	when (hdd /= HML.fromList [
		("alg", String "HS256"),
		("typ", String "JWT") ]) $ Left "BAD HEADER"
	when (iss /= "https://auth.login.yahoo.co.jp") $ Left "BAD ISS"
	when (aud /= cidToTxt cid) $ Left "BAD AUD"
	when (iat < now - 600) $ Left "BAD IAT"
	when (ex < now) $ Left "BAD EXP"
	when (n1 /= n0) $ Left "BAD NONCE"
	let sg1 = Signature . decodeUtf8
		. fst . BSC.spanEnd (== '=') . hmacSha256 (csToBs cs)
		$ encodeUtf8 hd <> "." <> encodeUtf8 pl
	when (sg1 /= sg) $ Left "BAD SIGNATURE"
	where
	hmacSha256 s d = B64.encode . convert $ hmacGetDigest (hmac s d :: HMAC SHA256)
	Iss iss = tbcIss tbc
	Aud aud = tbcAud tbc
	Iat iat = tbcIat tbc
	Exp ex = tbcExp tbc
	Nonce1 n1 = tbcNonce tbc
	sg = tbcSignature tbc
	Header hd = tbcHeader tbc
	Payload pl = tbcPayload tbc

newtype Iss = Iss Text deriving Show
newtype Aud = Aud Text deriving Show
newtype Now = Now POSIXTime deriving Show
newtype Iat = Iat POSIXTime deriving Show
newtype Exp = Exp POSIXTime deriving Show
newtype Nonce1 = Nonce1 Text deriving Show

newtype Signature = Signature Text deriving (Eq, Show)

newtype Header = Header Text deriving Show
newtype Payload = Payload Text deriving Show

headerToAeson :: Header -> Either String Aeson.Object
headerToAeson (Header t) = toAeson t

payloadToAeson :: Payload -> Either String Aeson.Object
payloadToAeson (Payload t) = toAeson t

toAeson :: Text -> Either String Aeson.Object
toAeson t = do
	b <- left ("B64.decode error: " ++)
		. B64.decode $ encodeUtf8 padded
	maybe (Left "Aeson.decode error") Right
		. Aeson.decode $ LBS.fromStrict b
	where
	padded = t <> Txt.replicate (3 - (Txt.length t - 1) `mod` 4) "="

newtype TokenType = TokenType Text deriving (Show, Eq)

data ToBeChecked = ToBeChecked {
	tbcTokenType :: TokenType,
	tbcIss :: Iss,
	tbcAud :: Aud,
	tbcIat :: Iat,
	tbcExp :: Exp,
	tbcNonce :: Nonce1,
	tbcHeader :: Header,
	tbcPayload :: Payload,
	tbcSignature :: Signature }

newtype Code = Code Text
newtype State = State Text
newtype Nonce0 = Nonce0 Text

codeToBs :: Code -> BS.ByteString
codeToBs (Code c) = encodeUtf8 c

lookupGetCode :: MyHandler (Maybe Code)
lookupGetCode = (Code <$>) <$> lookupGetParam "code"

lookupGetState :: MyHandler (Maybe State)
lookupGetState = (State <$>) <$> lookupGetParam "state"

getNonceFromState :: State -> MyHandler (Either String Nonce0)
getNonceFromState (State s) = do
	vs <- runDB $ do
		n <- select . from $ \sn -> do
			where_ $ sn ^. OpenIdStateNonceState ==. val s
			return ( sn ^. OpenIdStateNonceNonce )
		delete . from $ \sc -> do
			where_ $ sc ^. OpenIdStateNonceState ==. val s
		return n
	return $ case vs of
		[Value n] -> Right $ Nonce0 n
		_ -> Left "No or Multiple state"

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
