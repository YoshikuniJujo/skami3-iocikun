module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt

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
getHomeR :: Handler Html
getHomeR = do
	clientId <- lift $ Txt.concat . Txt.lines
		<$> Txt.readFile (directory </> "clientId.txt")
	redirectUri <- lift $ Txt.concat . Txt.lines
		<$> Txt.readFile (directory </> "redirectUri.txt")
	redirect $
		"https://auth.login.yahoo.co.jp/yconnect/v1/authorization?" <>
			"response_type=code+id_token&" <>
			"scope=openid+profile&" <>
			"client_id=" <> clientId <> "&state=hogeru&" <>
			"nonce=abcdefghijklmnop&" <>
			"redirect_uri=" <> redirectUri
--			"redirect_uri=http://localhost:3000/logined"
--			"redirect_uri=https://skami3.iocikun.jp/logined"

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
