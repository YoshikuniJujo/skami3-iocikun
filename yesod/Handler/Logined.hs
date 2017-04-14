{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Logined (getLoginedR) where

import Prelude (read)

import Data.Maybe (fromMaybe)
import qualified Data.Text as Txt

import Import (
	Maybe, String, Int, Text, Handler, Html,
	($), (.), (<$>), (<*>), (=<<),
	const, flip, fst, snd, uncurry, maybe, either, print,
	return, mapM_, show, putStrLn,
	setTitle, defaultLayout, widgetFile, redirect, runDB )
import OpenIdCon (
	UserId, AccessToken, authenticate, getProfile, showProfile, lookup,
	makeSession, makeAutoLogin )

import Model (Profile(..), EntityField(ProfileUserId))
import Database.Esqueleto

getLoginedR :: Handler Html
getLoginedR = do
	ua <- authenticate
	either (const $ return ()) (debugProfile . snd) ua
	either (const $ return ()) (makeSession . fst) ua
	either (const $ return ()) (makeAutoLogin . fst) ua
	either (const $ return ()) (setProfile . snd) ua
	redirect ("/" :: Text)
--	either showErrorPage (uncurry showPage) ua

setProfile :: AccessToken -> Handler ()
setProfile at = do
	prf <- getProfile at
	print prf
	let	uid = lookup "user_id" =<< prf
		n = lookup "name" =<< prf
		fn = lookup "family_name" =<< prf
		gn = lookup "given_name" =<< prf
		gd = lookup "gender" =<< prf
		bd :: Maybe Int = read . Txt.unpack <$> (lookup "birthday" =<< prf)
		em = lookup "email" =<< prf
	maybe (return ()) putStrLn uid
	maybe (return ()) putStrLn n
	maybe (return ()) putStrLn fn
	maybe (return ()) putStrLn gn
	maybe (return ()) putStrLn gd
	maybe (return ()) print bd
	maybe (return ()) putStrLn em
	let	mp = Profile <$> uid <*> n <*> fn <*> gn <*> gd <*> bd <*> em
	flip (maybe $ return ()) ((,) <$> mp <*> uid) $ \(prf, u) -> runDB $ do
		delete . from $ \p -> where_ $ p ^. ProfileUserId ==. val u
		_ <- insert prf
		return ()

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
