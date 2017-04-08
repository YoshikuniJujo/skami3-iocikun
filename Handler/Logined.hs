module Handler.Logined where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import Network.HTTP.Simple

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.ByteString.Base64.URL as B64

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HML

import qualified Data.Text as Txt
import qualified Data.ByteString.Lazy as LBS

import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)

import Data.ByteArray

directory :: FilePath
directory = "/home/tatsuya/keter/skami3/"

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getLoginedR :: Handler Html
getLoginedR = do
	Just code <- lookupGetParam "code"
	Just state <- lookupGetParam "state"
	$(logInfo) code
	$(logInfo) state
	clientId <- lift $ BS.concat . BSC.lines
		<$> BS.readFile (directory </> "clientId.txt")
	clientSecret <- lift $ BS.concat . BSC.lines
		<$> BS.readFile (directory </> "clientSecret.txt")
	redirectUri <- lift $ BS.concat . BSC.lines
		<$> BS.readFile (directory </> "redirectUri.txt")
	initReq <-
		parseRequest "https://auth.login.yahoo.co.jp/yconnect/v1/token"
	let	clientIdSecret = B64.encode $ clientId <> ":" <> clientSecret
		req = setRequestHeader "Content-Type"
			["application/x-www-form-urlencoded"]
			initReq { method = "POST" }
		req' = setRequestHeader "Authorization"
			["Basic " <> clientIdSecret] req
		req'' = setRequestBody (RequestBodyBS $
			"grant_type=authorization_code&code=" <>
			encodeUtf8 code <>
			"&redirect_uri=" <> redirectUri) req'
--			"&redirect_uri=http://localhost:3000/logined") req'
--			"&redirect_uri=https://skami3.iocikun.jp/logined") req'
	rBody <- getResponseBody <$> httpLBS req''
	liftIO $ LBS.writeFile (directory </> "tmp.txt") rBody
	let	Just resp = Aeson.decode rBody :: Maybe Aeson.Object
	print $ keys resp
	let	Just (String at) = HML.lookup "access_token" resp
		Just (String it) = HML.lookup "id_token" resp
	print at
	print it
	let	[hd, pl, sg] = Txt.splitOn "." it
	print hd
	print $ Txt.length hd
	print pl
	print $ Txt.length pl
	let	[Just hdd, Just pld] = map
			((Aeson.decode :: LBS.ByteString -> Maybe Aeson.Object)
				. LBS.fromStrict
				. either (error . ("B64.decode error " ++) . show) id
				. B64.decode . encodeUtf8)
			[padding hd, padding pl]
	print hdd
	print pld
	putStrLn sg
	lift . BSC.putStrLn . hmacSha256 clientSecret
		$ encodeUtf8 hd <> "." <> encodeUtf8 pl
	initReq2 <- parseRequest $
		"https://userinfo.yahooapis.jp/yconnect/v1/attribute?schema=openid"
	let	req2 = setRequestHeader
			"Authorization" ["Bearer " <> encodeUtf8 at] initReq2
	rBody2 <- getResponseBody <$> httpLBS req2
	let	Just json2 = Aeson.decode rBody2 :: Maybe Aeson.Object
	mapM_ print $ HML.toList json2
	(formWidget, formEnctype) <- generateFormPost sampleForm
	let	submission = Nothing :: Maybe FileForm
		handlerName = "getHomeR" :: Text
	defaultLayout $ do
		let (commentFormId, commentTextareaId, commentListId) = commentIds
		aDomId <- newIdent
		setTitle "Welcome To Yesod!"
		$(widgetFile "homepage")

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
