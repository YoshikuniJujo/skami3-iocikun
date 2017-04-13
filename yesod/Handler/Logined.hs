module Handler.Logined (getLoginedR) where

import qualified Data.Text as Txt

import Import (
	Either, String, Handler, Html,
	($), (.), (<$>),
	const, fst, snd, either, show, return,
	setTitle, defaultLayout, widgetFile )
import OpenIdCon (UserId, logined, debugProfile)

getLoginedR :: Handler Html
getLoginedR = do
	ua <- logined
	either (const $ return ()) (debugProfile . snd) ua
	showPage $ fst <$> ua

showPage :: Either String UserId -> Handler Html
showPage yid = do
	let	yourId = Txt.pack $ show yid
	defaultLayout $ do
		setTitle "Welcome To Skami3!"
		$(widgetFile "logined")
