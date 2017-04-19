module Handler.Logined (getLoginedR) where

import Prelude ((++))
import qualified Prelude as P (putStrLn)

import Import (
	Text, Handler, Html, HashMap, ($), (.), (<$>), (=<<), (>>=), (<>),
	maybe, fst, either, on, show, compare, sortBy,
	return, lift, map, mapM_, putStrLn, redirect )
import OpenIdConn (
	authenticate, makeSession, makeAutoLogin,
	getProfile, setProfile )

import qualified Data.Text as Txt
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HML

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

showProfile :: HashMap Text Aeson.Value -> [Text]
showProfile json = map ss . sortBy (compare `on` fst) . HML.toList $ json
	where
	ss (k, Aeson.String v) = k <> ": " <> v
	ss (k, v) = k <> ": " <> Txt.pack (show v)
