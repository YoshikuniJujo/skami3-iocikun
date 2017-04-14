{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Logined (getLoginedR) where

import Prelude (read)

import qualified Data.Text as Txt

import Import (
	Maybe, Int, Text, Handler, Html,
	($), (.), (<$>), (<*>), (=<<),
	const, flip, fst, snd, maybe, either, print,
	return, mapM_, putStrLn,
	redirect, runDB )
import OpenIdCon (
	AccessToken, authenticate, getProfile, showProfile, lookup,
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

setProfile :: AccessToken -> Handler ()
setProfile at = do
	profile <- getProfile at
	print profile
	let	uid = lookup "user_id" =<< profile
		n = lookup "name" =<< profile
		fn = lookup "family_name" =<< profile
		gn = lookup "given_name" =<< profile
		gd = lookup "gender" =<< profile
		bd :: Maybe Int = read . Txt.unpack
			<$> (lookup "birthday" =<< profile)
		em = lookup "email" =<< profile
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
