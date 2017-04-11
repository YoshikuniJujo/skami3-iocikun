module Handler.Logined where

import Import hiding ((==.), delete)
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

import Database.Esqueleto

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getLoginedR :: Handler Html
getLoginedR = do
	(clientId, clientSecret, redirectUri) <- lift $ (,,) 
		<$> getClientId <*> getClientSecret <*> getRedirectUri
	(Just code, Just state) <- (,)
		<$> lookupGetParam "code" <*> lookupGetParam "state"
	sn0 <- runDB . select . from $ \sn -> do
		where_ $ sn ^. OpenIdStateNonceState ==. val state
		return (
			sn ^. OpenIdStateNonceState,
			sn ^. OpenIdStateNonceNonce )
	(s0, n0) <- case sn0 of
		[(Value s, Value n)] -> return (s, n)
		_ -> error "BAD STATE"
	lift $ putStrLn "CHECK STATE"
	print $ s0 == state
	initReq <-
		parseRequest "https://auth.login.yahoo.co.jp/yconnect/v1/token"
	let	clientIdSecret = B64.encode
			$ cidToBs clientId <> ":" <> csToBs clientSecret
		req = setRequestHeader "Content-Type"
			["application/x-www-form-urlencoded"]
			initReq { method = "POST" }
		req' = setRequestHeader "Authorization"
			["Basic " <> clientIdSecret] req
		req'' = setRequestBody (RequestBodyBS $
			"grant_type=authorization_code&code=" <>
			encodeUtf8 code <>
			"&redirect_uri=" <> ruToBs redirectUri) req'
	rBody <- getResponseBody <$> httpLBS req''
	let	Just resp = Aeson.decode rBody :: Maybe Aeson.Object
	let	Just (String at) = HML.lookup "access_token" resp
		Just (String it) = HML.lookup "id_token" resp
		[hd, pl, sg] = Txt.splitOn "." it
	print $ keys resp
	let	[Just hdd, Just pld] = map
			((Aeson.decode :: LBS.ByteString -> Maybe Aeson.Object)
				. LBS.fromStrict
				. either (error . ("B64.decode error " ++) . show) id
				. B64.decode . encodeUtf8)
			[padding hd, padding pl]
	print hdd
	print pld
	let	Just (String n1) = lookup "nonce" pld
	putStrLn "CHECK NONCE"
	print $ n1 == n0
	when (n1 /= n0) $ error "BAD NONCE"
	runDB . delete . from $ \sc -> do
		where_ $ sc ^. OpenIdStateNonceState ==. val s0
	let sg1	= fst . BSC.spanEnd (== '=') . hmacSha256 (csToBs clientSecret)
		$ encodeUtf8 hd <> "." <> encodeUtf8 pl
	putStrLn "CHECK SIGNATURE"
	print $ sg1 == encodeUtf8 sg
	when (sg1 /= encodeUtf8 sg) $ error "BAD SIGNATURE"
	initReq2 <- parseRequest $
		"https://userinfo.yahooapis.jp/yconnect/v1/attribute?schema=openid"
	let	req2 = setRequestHeader
			"Authorization" ["Bearer " <> encodeUtf8 at] initReq2
	rBody2 <- getResponseBody <$> httpLBS req2
	let	Just json2 = Aeson.decode rBody2 :: Maybe Aeson.Object
--	mapM_ putStrLn . map showSimple $ HML.toList json2
	mapM_ print . map showSimple $ HML.toList json2
	(formWidget, formEnctype) <- generateFormPost sampleForm
	let	submission = Nothing :: Maybe FileForm
		handlerName = "getHomeR" :: Text
	defaultLayout $ do
		let (commentFormId, commentTextareaId, commentListId) = commentIds
		aDomId <- newIdent
		setTitle "Welcome To Skami3!"
		$(widgetFile "homepage")

showSimple :: (Text, Aeson.Value) -> Text
showSimple (k, String v) = k <> ": " <> v
showSimple _ = "not simple value"

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
