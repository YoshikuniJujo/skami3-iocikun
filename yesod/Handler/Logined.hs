module Handler.Logined (getLoginedR) where

import Prelude ((++))
import qualified Prelude as P

import Import (
	Text, Handler, Html, ($), (.), (<$>), (=<<), (>>=),
	maybe, either, return, lift, mapM_, putStrLn, redirect )
import OpenIdConn (
	authenticate, makeSession, makeAutoLogin,
	getProfile, setProfile, showProfile )

getLoginedR :: Handler Html
getLoginedR = do
	(authenticate >>=)
		. either (lift . P.putStrLn . ("getLoginedR : " ++))
		$ \(u, a) -> do
			mapM_ ($ u) [makeSession, makeAutoLogin]
			mapM_ ($ a) [
				debugProfile,
				(maybe (putStrLn eMsg) setProfile =<<)
					. getProfile ]
	redirect ("/" :: Text)
	where
	eMsg = "getLoginedR: error: can't getProfile"
	debugProfile at = maybe (return ()) (mapM_ putStrLn)
		=<< (showProfile <$>) <$> getProfile at
