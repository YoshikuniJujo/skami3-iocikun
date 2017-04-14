module Handler.Logined (getLoginedR) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as Txt

import Import (
	String, Handler, Html,
	($), (.), (<$>), (=<<),
	const, snd, uncurry, maybe, either,
	return, mapM_, show, putStrLn,
	setTitle, defaultLayout, widgetFile )
import OpenIdCon (
	UserId, AccessToken, authenticate, getProfile, showProfile, lookup,
	makeSession )

getLoginedR :: Handler Html
getLoginedR = do
	ua <- authenticate
	either (const $ return ()) (debugProfile . snd) ua
	makeSession
	either showErrorPage (uncurry showPage) ua

debugProfile :: AccessToken -> Handler ()
debugProfile at = maybe (return ()) (mapM_ putStrLn)
	=<< (showProfile <$>) <$> getProfile at

showPage :: UserId -> AccessToken -> Handler Html
showPage yid at = do
	let	yourId = Txt.pack $ show yid
	prf <- getProfile at
	let	yourName = fromMaybe "" $ lookup "name" =<< prf
		emailAddress = fromMaybe "" $ lookup "email" =<< prf
	defaultLayout $ do
		setTitle "Welcome To Skami3!"
		$(widgetFile "logined")

showErrorPage :: String -> Handler Html
showErrorPage em = do
	let	errorMessage = Txt.pack em
	defaultLayout $ do
		setTitle "Can't login"
		$(widgetFile "cantLogin")
