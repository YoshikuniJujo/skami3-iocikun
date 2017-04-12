module Handler.Logined where

import Import hiding ((==.), delete, UserId)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as BSC

import qualified Data.ByteString.Base64.URL as B64

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HML

import qualified Data.Text as Txt
import qualified Data.ByteString.Lazy as LBS

import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)

import Data.ByteArray
import Data.Scientific
import Data.Time.Clock.POSIX

import Database.Esqueleto

import OpenIdCon

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getLoginedR :: Handler Html
getLoginedR = do
	cs <- (\c s -> (,) <$> c <*> s)
		<$> lookupGetParam "code" <*> lookupGetParam "state"
	ua <- maybe (return Nothing) (uncurry logined) cs
	maybe (return ()) (debugProfile . snd) ua
	showPage $ fst <$> ua

logined :: Text -> Text -> Handler (Maybe (UserId, AccessToken))
logined code state = do
	(clientId, clientSecret, redirectUri) <- lift $ (,,) 
		<$> getClientId <*> getClientSecret <*> getRedirectUri
	sn0 <- runDB . select . from $ \sn -> do
		where_ $ sn ^. OpenIdStateNonceState ==. val state
		return (
			sn ^. OpenIdStateNonceState,
			sn ^. OpenIdStateNonceNonce )
	(s0, n0) <- case sn0 of
		[(Value s, Value n)] -> return (s, n)
		_ -> error "BAD STATE"

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
		at = AccessToken
			<$> (unstring =<< HML.lookup "access_token" resp)
		Just (String it) = HML.lookup "id_token" resp
		Just (String ei) = HML.lookup "expires_in" resp
		Just (String tt) = HML.lookup "token_type" resp
		Just (String rt) = HML.lookup "refresh_token" resp
		[hd, pl, sg] = Txt.splitOn "." it
	print $ keys resp
	print ei
	print tt
	print rt
	let	[Just hdd, Just pld] = map
			((Aeson.decode :: LBS.ByteString -> Maybe Aeson.Object)
				. LBS.fromStrict
				. either (error . ("B64.decode error " ++) . show) id
				. B64.decode . encodeUtf8)
			[padding hd, padding pl]
		iss = HML.lookup "iss" pld
		aud = unstring =<< HML.lookup "aud" pld
		iat = fromRational . toRational
			<$> (unnumber =<< HML.lookup "iat" pld)
		exp = fromRational . toRational
			<$> (unnumber =<< HML.lookup "exp" pld)
		uid = UserId <$> (unstring =<< HML.lookup "user_id" pld)
	now <- lift getPOSIXTime
	print hdd
	print pld
	print uid
	print iss
	print aud
	print clientId
	print (iat :: Maybe POSIXTime)
	print (exp :: Maybe POSIXTime)
	print now
	print $ maybe False (> now - 600) iat
	print $ maybe False (> now) exp
	let	Just (String n1) = lookup "nonce" pld
	when (n1 /= n0) $ error "BAD NONCE"
	when (iss /= Just (String "https://auth.login.yahoo.co.jp")) $
		error "BAD ISS"
	when (maybe True ((/= clientId) . ClientId) aud) $ error "BAD AUD"
	when (maybe True (< now - 600) iat) $ error "BAD IAT"
	when (maybe True (< now) exp) $ error "BAD EXP"
	runDB . delete . from $ \sc -> do
		where_ $ sc ^. OpenIdStateNonceState ==. val s0
	let sg1	= fst . BSC.spanEnd (== '=') . hmacSha256 (csToBs clientSecret)
		$ encodeUtf8 hd <> "." <> encodeUtf8 pl
	when (sg1 /= encodeUtf8 sg) $ error "BAD SIGNATURE"

	return $ (,) <$> uid <*> at


unstring :: Aeson.Value -> Maybe Text
unstring (String t) = Just t
unstring _ = Nothing

unnumber :: Aeson.Value -> Maybe Scientific
unnumber (Number n) = Just n
unnumber _ = Nothing

showPage :: Maybe UserId -> Handler Html
showPage yid = do
	(formWidget, formEnctype) <- generateFormPost sampleForm
	let	yourId = Txt.pack $ show yid
		submission = Nothing :: Maybe FileForm
		handlerName = "getHomeR" :: Text
	defaultLayout $ do
		let (commentFormId, commentTextareaId, commentListId) = commentIds
		aDomId <- newIdent
		setTitle "Welcome To Skami3!"
		$(widgetFile "homepage")

basicAuthentication ::
	IsString s => ClientId -> ClientSecret -> (s, [ByteString])
basicAuthentication cid cs = ("Authorization", ["Basic " <> mkClientIdSecret])
	where mkClientIdSecret = B64.encode $ cidToBs cid <> ":" <> csToBs cs

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 s d = B64.encode . convert $ hmacGetDigest (hmac s d :: HMAC SHA256)

padding :: Text -> Text
padding t = t <> Txt.replicate (3 - (Txt.length t - 1) `mod` 4) "="

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
