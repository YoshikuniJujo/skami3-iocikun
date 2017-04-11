module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt

import Crypto.Random

-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64

directory :: FilePath
directory = "/home/tatsuya/keter/skami3/"

getNonce :: MonadRandom m => Int -> m Text
getNonce = (Txt.dropWhileEnd (== '=') . decodeUtf8 . B64.encode <$>)
	. getRandomBytes

-- Define our data that will be used for creating the form.
data FileForm = FileForm {
	fileInfo :: FileInfo,
	fileDescription :: Text }

yconnect :: Text -> Text -> Handler Html
yconnect cid ruri = do
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
		"scope=openid+profile&" <>
		"client_id=" <> cid <> "&" <>
		"state=" <> stt <> "&" <>
		"nonce=" <> nnc <> "&" <>
		"redirect_uri=" <> ruri

getHomeR :: Handler Html
getHomeR = (uncurry yconnect =<<) . lift $ (,)
	<$> (Txt.concat . Txt.lines
		<$> Txt.readFile (directory </> "clientId.txt"))
	<*> (Txt.concat . Txt.lines
		<$> Txt.readFile (directory </> "redirectUri.txt"))

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

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
