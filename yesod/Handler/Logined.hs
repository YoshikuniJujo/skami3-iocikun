module Handler.Logined where

import Import hiding ((==.), delete, UserId)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import qualified Data.Text as Txt
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
